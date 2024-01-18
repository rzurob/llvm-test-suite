!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Structure Component: Scalar Sequence Derived Type Component
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

   type data
      sequence
      character(3) :: c
   end type

   type base
      sequence
      type(data) :: d1
      type(data) :: d2
   end type

   type container
      sequence
      type(base) :: b1
   end type

   integer :: stat
   character(150) :: msg

end module

module m1
   use m

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         type(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program structCompnt004
use m1

   type(container)               :: c1
   type(container), allocatable  :: c2

   c1 = container(b1=base(d1=data('ABC'),d2=data('DEF')))
   allocate ( c2 , source = container(b1=base(d1=data('GHI'),d2=data('JKL'))) )

   open (1, file = 'structCompnt004.1', form='formatted', access='sequential' )

   write ( 1, "(DT'_con1'(4,4))", iostat = stat, iomsg = msg )       c1%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
   msg = ''

   write ( 1, "(DT'_con2'(5,5),DT'_con3'(6,6))", iostat = stat, iomsg = msg )      ( c2%b1, i = 0 ,1 )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m

   type(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   write ( unit, * ) ' iotype:', iotype, ' v_list:', v_list

   write ( fmt, "(A2,I1,A2,I1,A1)" ) '(A', v_list(1),',A',v_list(2),')'
   write ( unit, fmt, iostat = iostat )    dtv%d1, dtv%d2

   iomsg = 'dtiowrite'

end subroutine
