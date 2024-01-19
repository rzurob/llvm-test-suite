!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Structure Component: Scalar Polymorphic Derived Type Component
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
      character(3) :: c = 'xxx'
   end type

   type, extends(base) :: child
      character(3) :: cc = 'xxx'
   end type

   type container1
      class(base), allocatable :: b1
   end type

   type container2
      class(child), pointer    :: b2
   end type

   integer :: stat
   character(150) :: msg

end module

program structCompnt002
use m

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

   type(container1)               :: c1
   class(container2), allocatable :: c2

   c1 = container1(base('ABC'))
   allocate( c2, source = container2(null()))
   allocate( c2%b2, source = child('DEF','GHI') )

   open (1, file = 'structCompnt002.1', form='formatted', access='sequential' )

   write ( 1, "(DT'_con1'(4))", iostat = stat, iomsg = msg )       c1%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, "(DT'_con2'(6,7),DT'_con2base'(8))", iostat = stat, iomsg = msg )     c2%b2, c2%b2%base
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   c1 = container1(child('JKL','MNO'))

   select type ( g => c1%b1 )
      type is ( child )
         write ( 1, "(DT'_con1'(7,8),DT'_con1base'(9))", iostat = stat, iomsg = msg )     c1%b1, g%base
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4
   end select

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
         write ( fmt, "(A2,I1,A1)" ) '(A', v_list(1),')'
         write ( unit, fmt, iostat = iostat )    dtv%c
      type is ( child )
         write ( fmt, "(A2,I1,A2,I1,A1)" ) '(A', v_list(1),',A',v_list(2),')'
         write ( unit, fmt, iostat = iostat )    dtv%c, dtv%cc
   end select
   iomsg = 'dtiowrite'

end subroutine
