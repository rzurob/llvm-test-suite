!*  ===================================================================
!*
!*  TEST CASE NAME             : final001kl
!*
!*  DATE                       : 2007-08-07 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Final Subroutine
!*                                    -  Ensure DTIO can be invoked during final subroutine
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type base (lb) ! lb=3
      integer, len :: lb
      character(lb) :: c = 'xxx'
      contains
         procedure, pass :: write => writeb
         generic :: write(formatted) => write
         final :: finalbase
   end type

   type, extends(base) :: child (kc) ! kc=4
      integer, kind :: kc
      integer(kc) :: i = -999
      contains
         procedure, pass :: write => writec
         final :: finalchild, finalchildrank1
   end type

   contains

      subroutine writeb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(20) :: fmt

         write (fmt, *) "(A", v_list(1), ")"
         write (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine writec (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(20) :: fmt

         write (fmt, *) "(A", v_list(1), ",1X, I",v_list(2),")"

         write (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      subroutine finalbase(dtv)
         type(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer :: stat
         character(200) :: msg

         write ( 1, "(DT(3))", iostat = stat, iomsg = msg )   dtv
         if ( ( stat /= 0 ) .or. (  msg /= 'dtiowriteb' ) )   error stop 1_4

      end subroutine

      subroutine finalchild(dtv)
         type(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer :: stat
         character(200) :: msg

         write ( 1, "(DT(3,5))", iostat = stat, iomsg = msg )   dtv
         if ( ( stat /= 0 ) .or. (  msg /= 'dtiowritec' ) )     error stop 2_4

      end subroutine

      subroutine finalchildrank1(dtv)
         type(child(*,4)), intent(inout) :: dtv(:) ! tcx: (*,4)
         integer :: stat
         character(200) :: msg

         write ( 1, "(DT(3,5))", iostat = stat, iomsg = msg )   dtv
         if ( ( stat /= 0 ) .or. (  msg /= 'dtiowritec' ) )     error stop 3_4

      end subroutine

end module

program final001kl
   use m

   integer :: stat
   character(200) :: msg

   type(base(:)), allocatable :: b1 ! tcx: (:)
   type(child(3,4)) :: c1 ! tcx: (3,4)
   class(child(:,4)), allocatable :: c2(:) ! tcx: (:,4)

   open ( 1, file = 'final001kl.1', form='formatted', access='sequential' )

   allocate (base(3):: b1 ) ! tcx: base(3)
   deallocate ( b1 )       !<- finalize b1 (base('xxx'))
   c1 = child(3,4)('abc',1001)  !<- finalize c1 (child('xxx',-999)) and child('abc',1001) and base('abc') ! tcx: (3,4)

   allocate (child(3,4):: c2(3) ) ! tcx: child(3,4)
   deallocate ( c2 )       !<- finalize c2 ((/child('xxx',-999), child('xxx',-999), child('xxx',-999)/)) and 3 base('xxx')

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 3 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 6 changes
