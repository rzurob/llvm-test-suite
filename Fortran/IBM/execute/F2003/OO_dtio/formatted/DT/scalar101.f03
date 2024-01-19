!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        scalar non-polymorphic derived type variable (read)
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
      integer(4) :: i = -9
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: idx
   character(10) :: rbuffer(3)

end module

program scalar101
use m

   type(base), allocatable :: b1
   type(base), pointer     :: b2
   type(base)              :: b3 = base()

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT)"

   open (1, file = 'scalar101.1', form='formatted', access='sequential' )

   allocate ( b1, source = base() )
   allocate ( b2, source = base() )

10 format (DT'b2')

   idx = 1

   read ( 1, fmt, iostat = stat, iomsg = msg )               b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read ( 1, 10, iostat = stat, iomsg = msg )                b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   read ( 1, "(DT'b3'(10))", iostat = stat, iomsg = msg )    b3
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   print *, rbuffer

   if ( ( b1%i /= 100 ) .or. ( b2%i /= 200 ) .or. ( b3%i /= 300 ) ) error stop 4_4

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, rbuffer, idx

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(20) :: fmt

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   if ( size(v_list) /= 0 ) then
      write (fmt, * ) '(I', v_list(1),')'
      read ( unit, fmt, iostat = iostat )    dtv%i
   else
      read ( unit, "(I4)", iostat = iostat ) dtv%i
   end if

   iomsg = 'dtioread'

end subroutine
