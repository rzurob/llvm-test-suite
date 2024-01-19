!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        multiple DT edit descriptor in an I/O statement (read)
!*                                        each item of different type
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
      integer(4)   :: i = -9
   end type

   type, extends(base) :: child
      integer(4)   :: j = -99
   end type

   type base1
      character(3) :: c = 'xxx'
   end type

   type base2
      sequence
      character(3) :: c = 'XXX'
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
      subroutine readformattedbase1(dtv, unit, iotype, v_list, iostat, iomsg )
         import base1
         class(base1), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
      subroutine readformattedbase2(dtv, unit, iotype, v_list, iostat, iomsg )
         import base2
         type(base2), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   character(10) :: rbuffer(4)

end module

program fdtedit108a
use m

   class(base), allocatable :: b1
   class(base), pointer     :: b2
   class(base1), pointer    :: b3
   type(base2)              :: b4 = base2()

   integer :: stat
   character(150) :: msg
   character(39) :: fmt = "(DT(1),DT'b2'(2),DT'b3'(3),DT'b4'(4,5))"

   allocate ( b1, source = base() )
   allocate ( b2, source = child() )
   allocate ( b3, source = base1() )

   open (1, file = 'fdtedit108a.1', form='formatted', access='sequential' )

   read ( 1, fmt, iostat = stat, iomsg = msg )      b1, b2, b3, b4
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )  error stop 1_4

   if ( b1%i /= 100 ) error stop 2_4
   select type ( b2 )
      type is ( child )
         if (( b2%i /= 200) .or. ( b2%j /= 201 ) ) error stop 3_4
   end select
   if ( b3%c /= 'IBM' ) error stop 4_4
   if ( b4%c /= 'FTN' ) error stop 5_4

   print *, rbuffer

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, rbuffer

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write ( rbuffer(v_list(1)), * ) iotype, v_list
   select type ( dtv )
      type is ( base )
         read ( unit, *, iostat = iostat ) dtv%i
      type is ( child )
         read ( unit, *, iostat = iostat ) dtv%i, dtv%j
   end select
   iomsg = 'dtioread'

end subroutine

subroutine readformattedbase1 (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base1, rbuffer

   class(base1), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write ( rbuffer(v_list(1)), * ) iotype, v_list
   read ( unit, *, iostat = iostat ) dtv%c

   iomsg = 'dtioread'

end subroutine

subroutine readformattedbase2 (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base2, rbuffer

   type(base2), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write ( rbuffer(v_list(1)), * ) iotype, v_list
   read ( unit, *, iostat = iostat ) dtv%c

   iomsg = 'dtioread'

end subroutine

