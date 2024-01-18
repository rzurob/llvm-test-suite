!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 21/03/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Array Constructor containing variables and structure constructor
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

   type base
      integer(4) :: i
   end type

   type, extends(base) :: child
      character(3) :: c = 'xxx'
   end type


   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: stat
   character(150) :: msg

end module

program arrayConstr002
use m

   type(base) :: b1 = base(102)
   type(child), allocatable :: c1

   class(base), allocatable :: b2
   class(base), pointer     :: b3(:)
   class(base), pointer     :: c2
   class(base), allocatable :: c3(:)

   open (1, file = 'arrayConstr002.1', form='formatted', access='sequential' )
   allocate ( c1, source = child(202, 'DEF') )

   write ( 1, "(DT'_base-1'(4),/,DT'_base-2'(5),/,DT'_base-3'(6))", iostat = stat, iomsg = msg )   (/ base(101), b1, base(103) /)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, "(DT'_child-1'(4,4),/,DT'_child-2'(5,5),/,DT'_child-3'(6,6))", iostat = stat, iomsg = msg )   (/ child(201,'ABC'), c1, child(203,'GHI') /)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   allocate ( b2, source = base(301) )
   allocate ( b3(3), source = (/ base(302), base(303), base(304) /) )

   write ( 1, "(DT'_base-1'(4))", iostat = stat, iomsg = msg )   (/ b2, b3 /)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

   allocate ( c2, source = child(401,'abc') )
   allocate ( c3(2), source = (/ child(402,'def'), child(403,'ghi') /) )

   write ( 1, "(DT'_child-1'(5,5),DT'_child-2'(6,6),DT'child_3'(7,7))", iostat = stat, iomsg = msg )   (/ ( c2, c3, i = 1,2 ) /)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 4_4

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   write ( unit, * ) ' iotype:', iotype, ' v_list:', v_list

   select type ( dtv )
      type is ( base )
         write ( fmt, "(A2,I1,A1)" ) '(I', v_list(1),')'
         write ( unit, fmt, iostat = iostat )    dtv%i
      type is ( child )
         write ( fmt, "(A2,I1,A2,I1,A1)" ) '(I', v_list(1),',A',v_list(2),')'
         write ( unit, fmt, iostat = iostat )    dtv%i, dtv%c
   end select
   iomsg = 'dtiowrite'

end subroutine
