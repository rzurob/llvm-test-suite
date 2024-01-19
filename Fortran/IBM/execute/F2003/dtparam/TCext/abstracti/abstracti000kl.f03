!*  ===================================================================
!*
!*                               by Alberto Alvarez-Mesquida)
!*  DATE                       : 2007-08-24 (original: 02/20/2006)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Abstract Interface
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*  scalar polymorphic derived type variable with abstract type
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

   type, abstract :: base (kbase_1) ! kbase_1=4
      integer, kind :: kbase_1
      integer(kbase_1) :: i
      contains
         procedure(inf), pass, deferred :: getc
   end type

   type, extends(base) :: child (lchild_1) ! lchild_1=3
      integer, len :: lchild_1
      character(lchild_1) :: c
      contains
         procedure, pass :: getc
   end type

   abstract interface
      character(3) function inf(dtv)
         import base
         class(base(4)), intent(in) :: dtv ! tcx: (4)
      end function
   end interface

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(in) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   contains

      character(3) function getc(dtv)
         class(child(4,*)), intent(in) :: dtv ! tcx: (4,*)
         getc = dtv%c
      end function

end module

program abstracti000kl
use m

   class(base(4)), pointer      :: b1 ! tcx: (4)
   class(child(4,:)), allocatable :: c1 ! tcx: (4,:)
   class(child(4,:)), pointer     :: c2 ! tcx: (4,:)

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT'c1'(10))"

   open (1, file = 'abstracti000kl.1', form='formatted', access='sequential' )

   allocate ( b1, source = child(4,3)(200,'abc') ) ! tcx: (4,3)
   allocate ( c1, source = child(4,3)(300,'def') ) ! tcx: (4,3)
   allocate ( c2, source = child(4,3)(400,'ghi') ) ! tcx: (4,3)

10 format (DT'b1')

   write ( 1, 10, iostat = stat, iomsg = msg )                b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, fmt, iostat = stat, iomsg = msg )    c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   write ( 1, "(DT'c2'(10,5))", iostat = stat, iomsg = msg )  c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

end program abstracti000kl

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base(4)), intent(in) :: dtv ! tcx: (4)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(20) :: fmt

   write ( unit, * ) 'iotype:', iotype

   select type ( dtv )
      type is (child(4,*)) ! tcx: (4,*)
         if ( size(v_list) == 0 ) then
            write ( unit, * ) ' empty v_list'
            write ( unit, *, iostat = iostat )      dtv%i, dtv%getc()
         else if ( size(v_list) == 1 ) then
            write ( unit, * ) ' v_list:',v_list
            write ( fmt, * ) '(I', v_list(1),')'
            write ( unit, fmt, iostat = iostat )    dtv%i
            write ( unit, *, iostat = iostat )      dtv%getc()
         else if ( size(v_list) == 2 ) then
            write ( unit, * ) ' v_list:',v_list
            write ( fmt, * ) '(I', v_list(1),',A',v_list(2),')'
            write ( unit, fmt, iostat = iostat )    dtv%i, dtv%getc()
         end if
   end select

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4)/declare with (4) - 4 changes
! type: child - added parameters (lchild_1) to invoke with (4,3)/declare with (4,*) - 7 changes
