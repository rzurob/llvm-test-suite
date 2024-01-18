!*  ===================================================================
!*
!*  TEST CASE NAME             : funcRetrn001kl
!*
!*  DATE                       : 2007-08-07 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Function Return
!*                                    -  function result is a (non-)polymorphic scalar entity with formatted i/o
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
         procedure, pass :: returnMe
   end type

   type, extends(base) :: child (kc) ! kc=4
      integer, kind :: kc
      integer(kc) :: i = -999
      contains
         procedure, pass :: write => writec
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

      class(base(3)) function returnMe(dtv) ! tcx: (3)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         allocatable :: returnMe

         allocate ( returnMe, source = dtv )

      end function

end module

program funcRetrn001kl
   use m

   integer :: stat
   character(200) :: msg

   type(base(:)), pointer     :: b1 ! tcx: (:)
   class(base(:)), allocatable :: b2 ! tcx: (:)
   class(child(:,4)), allocatable, target :: c1 ! tcx: (:,4)

!   procedure ( type(base(3)) ) :: returnMeExt ! tcx: (3)
    interface
        function returnMeExt(dtv)
            import base
            type(base(*)), intent(in) :: dtv
            type(base(3)) returnMeExt
        end function
    end interface

   allocate ( b1, source = base(3) ( 'abc' ) ) ! tcx: (3)
   allocate ( b2, source = base(3) ( 'def' ) ) ! tcx: (3)
   allocate ( c1, source = child(3,4) ( 'ghi', 10001 ) ) ! tcx: (3,4)

   open ( 1, file = 'funcRetrn001kl.1', form='formatted', access='sequential' )

   write ( 1, "(DT(3))", iostat = stat, iomsg = msg )    returnMeExt(b1)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 1_4

   write ( 1, "(DT(3))", iostat = stat, iomsg = msg )    returnMe(b2)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 2_4

   write ( 1, "(DT(3,5))", iostat = stat, iomsg = msg )  c1%returnMe()
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 3_4

end program

type(base(3)) function returnMeExt(dtv) ! tcx: (3)
  use m, only: base
  type(base(*)), intent(in) :: dtv ! tcx: (*)
  returnMeExt = dtv
end function


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 10 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 3 changes
