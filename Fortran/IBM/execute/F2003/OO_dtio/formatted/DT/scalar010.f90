!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        io-implied-do with scalar derived type variable
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

program scalar010
use m

   class(base), allocatable :: b1
   type(base), pointer     :: b2
   type(child)              :: b3 = child(300,301)

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT)"

   open (1, file = 'scalar010.1', form='formatted', access='sequential' )

   allocate ( b1, source = child(100,101) )
   allocate ( b2, source = base(200) )

10 format (DT'b2',i2)

   write ( 1, fmt, iostat = stat, iomsg = msg )              (b1, i = 1,3)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, 10, iostat = stat, iomsg = msg )               (b2,i, i = 3,1,-1)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   write ( 1, "(DT'b3-1'(10),DT'b3-2'(20))", iostat = stat, iomsg = msg )    ( b3, i= 10,13)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

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
         write ( unit, '(I4)', iostat = iostat )    dtv%i
      type is (child)
         write ( unit, '(I4,I4)', iostat = iostat )    dtv%i, dtv%j
   end select

   iomsg = 'dtiowrite'

end subroutine
