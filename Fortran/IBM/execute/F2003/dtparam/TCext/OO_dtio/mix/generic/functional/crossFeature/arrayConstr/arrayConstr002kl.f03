!*  ===================================================================
!*
!*  DATE                       : 2007-08-02 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Array Constructor
!*                                    -  Array constructor with allocatable components and io-implied-do formatted i/o
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

   type base (kb) ! kb=3
      integer, len :: kb
      character(kb), allocatable :: c
      contains
         procedure, pass :: write => writeb
         generic :: write(formatted) => write
   end type

   type, extends(base) :: child (kc) ! kc=4
      integer, kind :: kc
      integer(kc), allocatable :: i
      contains
         procedure, pass :: write => writec
   end type

   type, extends(child) :: gen3 (lg) ! lg=3
      integer, len :: lg
      character(lg), allocatable :: s
      contains
         procedure, pass :: write => writeg
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

      subroutine writeg (dtv, unit, iotype, v_list, iostat, iomsg)
         class(gen3(*,4,*)), intent(in) :: dtv ! tcx: (*,4,*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(30) :: fmt

         write (fmt, *) "(A", v_list(1), ",1X, I",v_list(2),",1X, A", v_list(3),")"

         write (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i, dtv%s

         iomsg = 'dtiowriteg'

      end subroutine

end module

program arrayConstr002kl
   use m

   integer :: stat
   character(200) :: msg

   open ( 1, file = 'arrayConstr002kl.1', form='formatted', access='sequential' )

   write ( 1, "(4(DT(3)))", iostat = stat, iomsg = msg )        (/ base(3)('abc'), base(3)('def'), ( base(3)('ghi'), i=1,2 )/) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 101_4

   write ( 1, "(3(DT(3,4)))", iostat = stat, iomsg = msg )      (/ ( child(3,4)('IBM',2005),i=1,3 ) /) ! tcx: (3,4)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 2_4

   write ( 1, "(3(DT(3,5,3)))", iostat = stat, iomsg = msg )    (/ ( gen3(3,4,3)('FTN',2003,'GRT'),i=4,6 ) /) ! tcx: (3,4,3)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) ) error stop 3_4

   write ( 1, "(DT(3),DT(4),DT(5))", iostat = stat, iomsg = msg )  ( (/ ( base(3)('ghi'), i=1,2 ) /) , i=0,1 ) ! tcx: (3)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 4_4

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (3) / declare with (*) - 5 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 2 changes
! type: gen3 - added parameters (lg) to invoke with (3,4,3) / declare with (*,4,*) - 2 changes