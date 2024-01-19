!*  ===================================================================
!*
!*  DATE                       : 2007-08-07 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Function Return
!*                                    -  function result is a (non-)polymorphic array entity with formatted i/o
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

   type base (lbase1) ! lbase1=3
      integer, len :: lbase1
      character(lbase1) :: c = 'xxx'
      contains
         procedure, pass :: write => writeb
         generic :: write(formatted) => write
   end type

   type, extends(base) :: child (kchild1) ! kchild1=4
      integer, kind :: kchild1
      integer(kchild1) :: i = -999
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
         class(base(*)), intent(in) :: dtv(:) ! tcx: (*)
         allocatable :: returnMe(:)

         allocate ( returnMe(size(dtv,1)), source = dtv )

      end function

end module

program funcRetrn002kl
   use m

   integer :: stat
   character(200) :: msg

   type(base(:)), pointer     :: b1(:) ! tcx: (:)
   class(base(:)), allocatable :: b2(:) ! tcx: (:)
   class(child(:,4)), allocatable, target :: c1(:) ! tcx: (:,4)

   interface
      type(base(3)) function returnMeExt(dtv) ! tcx: (3)
         import base
         type(base(*)), intent(in) :: dtv(3) ! tcx: (*)
         dimension :: returnMeExt(3)
      end function
   end interface

   allocate ( b1(3), source = (/ base(3)('abc'), base(3)('def'), base(3)('ghi') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate ( b2(4), source = (/ base(3)('ABC'), base(3)('DEF'), base(3)('GHI'), base(3)('JKL') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate ( c1(3), source = (/ child(3,4) ( 'abc', 10001 ), child(3,4) ( 'def', 10002 ), child(3,4) ( 'ghi', 10003 ) /) ) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)

   open ( 1, file = 'funcRetrn002kl.1', form='formatted', access='sequential' )

   write ( 1, "(DT(3))", iostat = stat, iomsg = msg )       returnMeExt(b1)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )        error stop 1_4

   write ( 1, "(DT(3))", iostat = stat, iomsg = msg )       returnMe(b2)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )        error stop 2_4

   write ( 1, "(3(DT(3,6)))", iostat = stat, iomsg = msg )  returnMe(c1)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )        error stop 3_4

end program

type(base(3)) function returnMeExt(dtv) ! tcx: (3)
  use m, only: base
  type(base(*)), intent(in) :: dtv(3) ! tcx: (*)
  dimension :: returnMeExt(3)
  returnMeExt = dtv
end function


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase1) to invoke with (3) / declare with (*) - 16 changes
! type: child - added parameters (kchild1) to invoke with (3,4) / declare with (*,4) - 5 changes
