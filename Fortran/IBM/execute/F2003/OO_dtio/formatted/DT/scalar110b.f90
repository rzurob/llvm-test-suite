!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        io-implied-do inside DTIO for derived type components (read)
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
      type(data), allocatable :: d(:)
      integer(4) :: k
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
   character(20) :: rbuffer(10)

end module

program scalar110b
use m

   class(base), allocatable :: b1
   type(base), pointer      :: b2
   type(base)               :: b3

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT'_b1')"

   open (1, file = 'scalar110b.1', form='formatted', access='sequential' )

   allocate ( b1, source = base((/ data(-999), data(-999) /) , -999) )
   allocate ( b2, source = base((/ data(-999), data(-999) /) , -999) )
   b3 = base((/ data(-999), data(-999), data(-999)         /) , -999)

   idx = 1

   read ( 1, fmt, iostat = stat, iomsg = msg )                 b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read ( 1, *, iostat = stat, iomsg = msg )                   b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   read ( 1, '(DT"_b3"(1))', iostat = stat, iomsg = msg )      b3
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   print *, rbuffer

   if ( ( b1%d(1)%i /= 100 ) .or. ( b1%d(2)%i /= 101 ) .or. ( b1%k /= 1001 ) ) error stop 4_4
   if ( ( b2%d(1)%i /= 200 ) .or. ( b2%d(2)%i /= 201 ) .or. ( b2%k /= 2002 ) ) error stop 5_4
   if ( ( b3%d(1)%i /= 300 ) .or. ( b3%d(2)%i /= 301 ) .or. ( b3%d(3)%i /= 302 ) .or.  ( b3%k /= 3003 ) ) error stop 6_4

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, data, rbuffer, idx

   interface read(formatted)
      subroutine readformatteddata(dtv, unit, iotype, v_list, iostat, iomsg )
         import data
         class(data), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(60) :: fmt

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   write ( fmt, * ) '(', ( 'DT(',i,'),',i=1,size(dtv%d)-1 ) ,' DT(', size(dtv%d), '), 1X, I4)'
   read ( unit, fmt, iomsg = iomsg ) (dtv%d(i),i=1,size(dtv%d)), dtv%k

   if ( iomsg /= 'dataread' ) error stop 7_4

   iomsg = 'dtioread'

end subroutine

subroutine readformatteddata (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: data, rbuffer, idx

   class(data), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(40) :: fmt = ''

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   read ( unit, "(/)" )
   read ( unit, "(I4)" )  dtv%i

   iomsg = 'dataread'

end subroutine


