!*  ===================================================================
!*
!*  DATE                       : 2007-06-11 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        scalar polymorphic derived type variable
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

   type base (kb)
      integer, kind :: kb
      integer(kb) :: i
   end type

   type, extends(base) :: child (lc)
      integer, len :: lc
      character(lc) :: c
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program scalar002kl
use m

   class(base(4)), allocatable  :: b1
   class(base(4)), pointer      :: b2
   class(child(4,:)), allocatable :: c1
   class(child(4,:)), pointer     :: c2

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT)"

   open (1, file = 'scalar002kl.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(4)(100) )
   allocate ( b2, source = child(4,3)(200,'abc') )
   allocate ( c1, source = child(4,3)(300,'def') )
   allocate ( c2, source = child(4,3)(400,'ghi') )

10 format (DT'b2')

   write ( 1, fmt, iostat = stat, iomsg = msg )               b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, 10, iostat = stat, iomsg = msg )                b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   write ( 1, "(DT'c1'(10))", iostat = stat, iomsg = msg )    c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

   write ( 1, "(DT'c2'(10,5))", iostat = stat, iomsg = msg )  c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 4_4

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(20) :: fmt

   write ( unit, * ) 'iotype:', iotype

   select type ( dtv )
      type is (base(4))
         write ( unit, * ) ' empty v_list'
         write ( unit, "(I4)", iostat = iostat ) dtv%i
      type is (child(4,*))
         if ( size(v_list) == 0 ) then
            write ( unit, * ) ' empty v_list'
            write ( unit, *, iostat = iostat ) dtv%i, dtv%c
         else if ( size(v_list) == 1 ) then
            write ( unit, * ) ' v_list:',v_list
            write ( fmt, * ) '(I', v_list(1),')'
            write ( unit, fmt, iostat = iostat )    dtv%i
            write ( unit, *, iostat = iostat )      dtv%c
         else if ( size(v_list) == 2 ) then
            write ( unit, * ) ' v_list:',v_list
            write ( fmt, * ) '(I', v_list(1),',A',v_list(2),')'
            write ( unit, fmt, iostat = iostat )    dtv%i, dtv%c
         end if
   end select

   iomsg = 'dtiowrite'

end subroutine
