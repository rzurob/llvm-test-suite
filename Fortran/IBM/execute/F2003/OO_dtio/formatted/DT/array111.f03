!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        zero-sized array and array of zero storage derived type (read)
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

   type zerobase
   end type

   type base
      integer i
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
      subroutine readformattedzerobase(dtv, unit, iotype, v_list, iostat, iomsg )
         import zerobase
         class(zerobase), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: idx
   character(15) :: rbuffer(9)

end module

program array111
use m

   class(base), allocatable :: b1(:)
   type(base)               :: b2(0)

   class(zerobase), allocatable :: z1(:)
   type(zerobase)               :: z2(4)

   integer :: stat
   character(150) :: msg
   character(31) :: fmt = "(DT//)"

   open (1, file = 'array111.1', form='formatted', access='sequential' )

   allocate ( b1(1:0) )
   allocate ( z1(5) )

10 format (DT//)
   msg = ''

   idx = 1

   read ( 1, fmt, iostat = stat, iomsg = msg )               b1
   if ( ( stat /= 0 ) .or. ( msg /= '' ) ) error stop 1_4

   read ( 1, 10, iostat = stat, iomsg = msg )                b2
   if ( ( stat /= 0 ) .or. ( msg /= '' ) ) error stop 2_4

   read ( 1, "(DT'z1-1'(1,2),/,DT'z1-2'(-3,-4),/,DT'z1-3'(5,6),/,DT'z1-4'(-7,-8),/,DT'z1-5'(9,10))", iostat = stat, iomsg = msg )   z1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   read ( 1, "(DT'z2-1'(11,12),DT'z2-2'(-13,-14))", iostat = stat, iomsg = msg )   z2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 4_4

   print *, rbuffer

end program

subroutine readformattedzerobase (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: zerobase, rbuffer, idx

   class(zerobase), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   iomsg = 'dtioread'

end subroutine

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, rbuffer, idx

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   iomsg = 'dtioread'

end subroutine
