!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Array Constructor for sequence type
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
      sequence
      integer(4) :: i
      character(3) :: c
   end type

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

   integer :: stat
   character(150) :: msg

end module

program arrayConstr003
use m

   type(base) :: b1 = base(102,'def')
   type(base) :: b2(3) = (/ base(201,'abc'), base(202,'def'), base(203,'ghi') /)

   open (1, file = 'arrayConstr003.1', form='formatted', access='sequential' )

   write ( 1, "(DT'_base-1'(4,4),/,DT'_base-2'(5,5),/,DT'_base-3'(6,6))", iostat = stat, iomsg = msg )   (/ base(101,'abc'), b1, base(103,'ghi') /)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, "(DT'_base-1'(5,5))", iostat = stat, iomsg = msg )   (/ ( b2(i), base(i+300,'ibm'), i = 1, 3 ) /)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4


end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   type(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   write ( unit, * ) ' iotype:', iotype, ' v_list:', v_list

   write ( fmt, "(A2,I1,A2,I1,A1)" ) '(I', v_list(1),',A',v_list(2),')'
   write ( unit, fmt, iostat = iostat )    dtv%i, dtv%c

   iomsg = 'dtiowrite'

end subroutine
