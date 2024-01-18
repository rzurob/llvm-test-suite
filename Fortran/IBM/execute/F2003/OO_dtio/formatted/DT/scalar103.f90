!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        scalar (non-)polymorphic derived type variable with non-polymorphic component (read)
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

program scalar103
use m

   class(base), allocatable :: b1
   type(base), pointer      :: b2
   type(base)               :: b3 = base(data(-999), -999)

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT(5,6))"

   open (1, file = 'scalar103.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(data(-999), -999) )
   allocate ( b2, source = base(data(-999), -999) )

10 format (DT'b2'(7,8) )

   idx = 1

   read ( 1, fmt, iostat = stat, iomsg = msg )               b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read ( 1, 10, iostat = stat, iomsg = msg )                b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   read ( 1, "(DT'b3'(9,10))", iostat = stat, iomsg = msg )   b3
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   print *, rbuffer

   if ( ( b1%d%i /= 101 ) .or. ( b1%j /= 1001 ) )  error stop 4_4
   if ( ( b2%d%i /= 102 ) .or. ( b2%j /= 1002 ) )  error stop 5_4
   if ( ( b3%d%i /= 103 ) .or. ( b3%j /= 1003 ) )  error stop 6_4


end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, data, rbuffer, idx

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(30) :: fmt

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   if ( size(v_list) /= 2 ) error stop 7_4

   write ( fmt, * )   "( I", v_list(1), ", I", v_list(2), ")"
   read ( unit, fmt ) dtv%d, dtv%j

   iomsg = 'dtioread'

end subroutine
