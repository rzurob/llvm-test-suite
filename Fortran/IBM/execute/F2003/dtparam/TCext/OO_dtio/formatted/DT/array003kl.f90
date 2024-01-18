!*  ===================================================================
!*
!*  DATE                       : 2007-06-04 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
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

   type data (k)
      integer, kind :: k
      integer(k) :: i
   end type

   type base (kb)
      integer, kind :: kb
      type(data(kb)) :: d
      integer(kb) :: j
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

program array003kl
use m

   class(base(4)), allocatable :: b1(:)
   type(base(4)), pointer      :: b2(:,:)
   type(base(4))               :: b3(3) = (/ base(4)(data(4)(301), 3001), base(4)(data(4)(302), 3002), base(4)(data(4)(303), 3003) /)

   integer :: stat
   character(150) :: msg
   character(31) :: fmt = "(DT'_b1-1'(5,6),DT'_b1-2'(6,7))"

   open (1, file = 'array003kl.1', form='formatted', access='sequential' )

   allocate ( b1(4), source = (/ base(4)(data(4)(101_4), 1001_4), base(4)(data(4)(102_4), 1002_4), &
                                 base(4)(data(4)(103_4), 1003_4), base(4)(data(4)(104_4), 1004_4) /) )

   allocate ( b2(2,2), source = reshape ( source = (/ base(4)(data(4)(201_4), 2001_4), base(4)(data(4)(202_4), 2002_4), &
                                                      base(4)(data(4)(203_4), 2003_4), base(4)(data(4)(204_4), 2004_4) /), shape = (/2,2/) ) )

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

   class(base(4)), intent(in) :: dtv
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
