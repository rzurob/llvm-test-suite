!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        array (non-)polymorphic derived type variable with non-polymorphic component
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
      integer(4) :: i
   end type

   type base
      type(data) :: d
      integer(4) :: j
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

end module

program array003
use m

   class(base), allocatable :: b1(:)
   type(base), pointer      :: b2(:,:)
   type(base)               :: b3(3) = (/ base(data(301), 3001), base(data(302), 3002), base(data(303), 3003) /)

   integer :: stat
   character(150) :: msg
   character(31) :: fmt = "(DT'_b1-1'(5,6),DT'_b1-2'(6,7))"

   open (1, file = 'array003.1', form='formatted', access='sequential' )

   allocate ( b1(4), source = (/ base(data(101_4), 1001_4), base(data(102_4), 1002_4), &
                                 base(data(103_4), 1003_4), base(data(104_4), 1004_4) /) )

   allocate ( b2(2,2), source = reshape ( source = (/ base(data(201_4), 2001_4), base(data(202_4), 2002_4), &
                                                      base(data(203_4), 2003_4), base(data(204_4), 2004_4) /), shape = (/2,2/) ) )

10 format (DT'_b2-11'(7,8),DT'_b2-21'(8,9),DT'_b2-12'(7,8),DT'_b2-22'(8,9) )

   write ( 1, fmt, iostat = stat, iomsg = msg )               b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, 10, iostat = stat, iomsg = msg )                b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   write ( 1, "(DT'b3-1'(9,10),:,/)", iostat = stat, iomsg = msg )   b3  !<- each element will be double spaced
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, data

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(30) :: fmt

   if ( size(v_list) /= 2 ) error stop 4_4

   write ( fmt, * ) "( A, I4, I4, I", v_list(1), ", I", v_list(2), ")"
   write ( unit, fmt ) iotype, v_list, dtv%d, dtv%j

   iomsg = 'dtiowrite'

end subroutine
