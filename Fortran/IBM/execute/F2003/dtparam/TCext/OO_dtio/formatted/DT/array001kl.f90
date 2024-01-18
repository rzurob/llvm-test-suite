!*  ===================================================================
!*
!*  TEST CASE NAME             : array001kl
!*
!*  DATE                       : 2007-06-01 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        array non-polymorphic derived type variable
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

   type base (k)
      integer, kind :: k
      integer(k) :: i
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

program array001kl
use m

   type(base(4)), allocatable :: b1(:)
   type(base(4)), pointer     :: b2(:,:)
   type(base(4))              :: b3(4) = (/ base(4)(300), base(4)(301), base(4)(302), base(4)(303) /)

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT)"

   open (1, file = 'array001kl.1', form='formatted', access='sequential' )

   allocate ( b1(3), source = (/ base(4)(100), base(4)(101), base(4)(102) /) )
   allocate ( b2(2,2), source = reshape ( source = (/  base(4)(200), base(4)(201), base(4)(202), base(4)(203) /) , shape = (/2,2/) ) )

10 format (DT'b2-1'(5),DT'b2-2'(10))

   write ( 1, fmt, iostat = stat, iomsg = msg )               b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, 10, iostat = stat, iomsg = msg )                b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   write ( 1, "(DT'b3-1'(5), DT'b3-2'(10), DT'b3-3'(15))", iostat = stat, iomsg = msg )    b3
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(20) :: fmt

   write ( unit, * ) 'iotype:', iotype

   if ( size(v_list) /= 0 ) then
      write ( unit, * ) ' v_list:',v_list
      write ( fmt, * ) '(I', v_list(1),')'
      write ( unit, fmt, iostat = iostat )    dtv%i
   else
      write ( unit, * ) ' empty v_list'
      write ( unit, "(I4)", iostat = iostat ) dtv%i
   end if


   iomsg = 'dtiowrite'

end subroutine
