!*  ===================================================================
!*
!*  DATE                       : 2007-06-04 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        array sequence derived type variable containing sequence components
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

   type data (kd)
      integer, kind :: kd
      sequence
      integer(kd) :: i
   end type

   type base (kb,lb)
      integer, kind :: kb
      integer, len :: lb
      sequence
      type(data(kb))   :: d
      character(lb) :: c
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         type(base(4,*)), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program array006kl
use m

   type(base(4,3)), allocatable, target :: b1(:)
   type(base(4,3)), pointer     :: b2(:)
   type(base(4,3))              :: b3(3) = (/ base(4,3)(data(4)(300),'abc'), base(4,3)(data(4)(301),'def'), base(4,3)(data(4)(303),'ghi') /)

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT'b1-1'(5),DT'b1-2'(6))"

   open (1, file = 'array006kl.1', form='formatted', access='sequential' )

   allocate ( b1(4), source = (/ base(4,3)(data(4)(100),'abc'), base(4,3)(data(4)(101),'def'), base(4,3)(data(4)(102),'ghi'), base(4,3)(data(4)(103),'jkl') /) )
   b2 => b1(1:3:2)

10 format (DT'b2-1',/,DT'b2-2'(7))

   write ( 1, fmt, iostat = stat, iomsg = msg )               b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, 10, iostat = stat, iomsg = msg )                b2(2:1:-1)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   write ( 1, "(DT'b3'(10))", iostat = stat, iomsg = msg )    b3
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   type(base(4,*)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(20) :: fmt

   write ( unit, * ) 'iotype:', iotype

   if ( size(v_list) /= 0 ) then
      write ( unit, * ) ' v_list:',v_list
      write ( fmt, * ) '(I', v_list(1),', 1X,A3 )'
      write ( unit, fmt, iostat = iostat )          dtv%d, dtv%c
   else
      write ( unit, * ) ' empty v_list'
      write ( unit, "(I4,1X,A3)", iostat = iostat ) dtv%d, dtv%c
   end if

   iomsg = 'dtiowrite'

end subroutine