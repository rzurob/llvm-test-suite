!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        scalar zero storage derived type
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
      character(0) :: c
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

program scalar011
use m

   type(base), allocatable :: b1
   type(base), pointer     :: b2
   type(base)              :: b3 = base(c='')

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT)"

   open (1, file = 'scalar011.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(c='') )
   allocate ( b2, source = base(c='') )

10 format (DT'b2')

   write ( 1, fmt, iostat = stat, iomsg = msg )               b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, 10, iostat = stat, iomsg = msg )                b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   write ( 1, "(DT'b3'(10))", iostat = stat, iomsg = msg )    b3
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

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
      write ( unit, "(A)", iostat = iostat )    dtv%c
   else
      write ( unit, * ) ' empty v_list'
      write ( unit, "(A)", iostat = iostat )    dtv%c
   end if


   iomsg = 'dtiowrite'

end subroutine
