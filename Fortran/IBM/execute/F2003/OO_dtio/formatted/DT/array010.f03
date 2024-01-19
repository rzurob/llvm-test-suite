!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        io-implied-do with array derived type variable
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

   type, extends( base ) :: child
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

program array010
use m

   class(base), allocatable :: b1(:)
   type(base), pointer      :: b2(:,:)
   type(child)              :: b3(6:9) = (/ child(300,3000), child(301,3001), child(302,3002), child(303,3003) /)

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT(-1),/, DT(-2),/, DT(-3))"

   open (1, file = 'array010.1', form='formatted', access='sequential' )

   allocate ( b1(3), source = (/ child(100,1000), child(101,1001), child(102,1002) /) )
   allocate ( b2(2,2), source = reshape ( source = (/ base(200), base(201), base(202), base(203) /), shape = (/2,2/) ) )

10 format (DT'b2-1'(-2,-4),/,DT'b2-2'(-6,-8))
20 format (DT'b3-1'(10),DT'b3-2'(20))

   write ( 1, fmt, iostat = stat, iomsg = msg )        (b1, i = 1,3)  !<- write the array 3 times
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )    error stop 1_4
   msg = ''
   write ( 1, 10, iostat = stat, iomsg = msg )         (b2(1,i), b2(2,i), i = 1,2)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )    error stop 2_4
   msg = ''
   write ( 1, 20, iostat = stat, iomsg = msg )         (b3(i), i= 6,9)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )    error stop 3_4

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(20) :: fmt

   write ( unit, * ) 'iotype:', iotype

   if ( size(v_list) /= 0 ) then
      write ( unit, * ) ' v_list:',v_list
   else
      write ( unit, * ) ' empty v_list'
   end if

   select type ( dtv )
      type is (base)
         write ( unit, '(I4)', iostat = iostat )     dtv%i
      type is (child)
         write ( unit, '(I4,I5)', iostat = iostat )  dtv%i, dtv%j
   end select

   iomsg = 'dtiowrite'

end subroutine
