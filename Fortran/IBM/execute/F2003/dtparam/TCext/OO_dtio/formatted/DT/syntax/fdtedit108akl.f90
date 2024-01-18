!*  ===================================================================
!*
!*  DATE                       : 2007-06-08 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
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
   type base (kb)
      integer, kind :: kb
      integer(kb)   :: i = -9
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc)   :: j = -99
   end type

   type base1 (lb1)
      integer, len :: lb1
      character(lb1) :: c = 'xxx'
   end type

   type base2 (lb2)
      integer, len :: lb2
      sequence
      character(lb2) :: c = 'XXX'
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
      subroutine readformattedbase1(dtv, unit, iotype, v_list, iostat, iomsg )
         import base1
         class(base1(*)), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
      subroutine readformattedbase2(dtv, unit, iotype, v_list, iostat, iomsg )
         import base2
         type(base2(*)), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   character(10) :: rbuffer(4)

end module

program fdtedit108akl
use m

   class(base(4)), allocatable :: b1
   class(base(4)), pointer     :: b2
   class(base1(:)), pointer    :: b3
   type(base2(3))              :: b4 = base2(3)()

   integer :: stat
   character(150) :: msg
   character(39) :: fmt = "(DT(1),DT'b2'(2),DT'b3'(3),DT'b4'(4,5))"

   allocate ( b1, source = base(4)() )
   allocate ( b2, source = child(4,4)() )
   allocate ( b3, source = base1(3)() )

   open (1, file = 'fdtedit108akl.1', form='formatted', access='sequential' )

   read ( 1, fmt, iostat = stat, iomsg = msg )      b1, b2, b3, b4
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )  error stop 1_4

   if ( b1%i /= 100 ) error stop 2_4
   select type ( b2 )
      type is ( child(4,4) )
         if (( b2%i /= 200) .or. ( b2%j /= 201 ) ) error stop 3_4
   end select
   if ( b3%c /= 'IBM' ) error stop 4_4
   if ( b4%c /= 'FTN' ) error stop 5_4

   print *, rbuffer

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, rbuffer

   class(base(4)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write ( rbuffer(v_list(1)), * ) iotype, v_list
   select type ( dtv )
      type is ( base(4) )
         read ( unit, *, iostat = iostat ) dtv%i
      type is ( child(4,4) )
         read ( unit, *, iostat = iostat ) dtv%i, dtv%j
   end select
   iomsg = 'dtioread'

end subroutine

subroutine readformattedbase1 (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base1, rbuffer

   class(base1(*)), intent(inout) :: dtv
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

   type(base2(*)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write ( rbuffer(v_list(1)), * ) iotype, v_list
   read ( unit, *, iostat = iostat ) dtv%c

   iomsg = 'dtioread'

end subroutine

