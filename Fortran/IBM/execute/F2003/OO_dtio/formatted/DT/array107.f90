!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 21/03/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        array sequence derived type variable containing sequence components which uses DTIO (read)
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
      sequence
      integer(4) :: i
   end type

   type base
      sequence
      type(data)   :: d
      character(3) :: c
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         type(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: idx
   character(15) :: rbuffer(22)

end module

program array107
use m

   type(base), allocatable :: b1(:,:)
   type(base), pointer     :: b2(:)
   type(base)              :: b3(2,2)

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(4(DT(5),/))"

   open (1, file = 'array107.1', form='formatted', access='sequential' )

   allocate ( b1(2,2) )
   allocate ( b2(3) )

10 format (2(DT'b2'(6,7),/), DT'b2-3'(7,8))

   idx = 1

   read ( 1, fmt, iostat = stat, iomsg = msg )                 b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read ( 1, 10, iostat = stat, iomsg = msg )                  b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   read ( 1, "(4(DT'b3'(8,9),/))", iostat = stat, iomsg = msg )     b3(1:2,2:1:-1)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   if ( ( b1(1,1)%d%i  /= 100 )   .or. ( b1(2,1)%d%i  /= 101 )   .or. ( b1(1,2)%d%i  /= 102 )  .or. ( b1(2,2)%d%i  /= 103 ) .or. &
        ( b1(1,1)%c  /= 'abc' ) .or. ( b1(2,1)%c  /= 'def' ) .or. ( b1(1,2)%c  /= 'ghi' ).or. ( b1(2,2)%c  /= 'jkl' )  ) error stop 4_4

   if ( ( b2(1)%d%i  /= 200 ) .or. ( b2(2)%d%i  /= 201 ) .or. ( b2(3)%d%i  /= 202 ) .or.  &
        ( b2(1)%c  /= 'ABC' ) .or. ( b2(2)%c  /= 'DEF' ) .or. ( b2(3)%c  /= 'GHI' ) ) error stop 5_4

   if ( ( b3(1,1)%d%i  /= 300 )   .or. ( b3(2,1)%d%i  /= 301 )   .or. ( b3(1,2)%d%i  /= 302 )  .or. ( b3(2,2)%d%i  /= 303 ) .or. &
        ( b3(1,1)%c  /= 'abc' ) .or. ( b3(2,1)%c  /= 'DEF' ) .or. ( b3(1,2)%c  /= 'ghi' ).or. ( b3(2,2)%c  /= 'JKL' )  ) error stop 6_4
   
   print *, rbuffer

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, data, rbuffer, idx

   interface read(formatted)
      subroutine readformatteddata(dtv, unit, iotype, v_list, iostat, iomsg )
         import data
         type(data), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   type(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   iomsg = ''

   if ( size(v_list) == 1 ) then
      write ( fmt, * ) '(I', v_list(1),', 1X,A3 )'
      read ( unit, fmt, iostat = iostat, iomsg = iomsg )    dtv%d%i, dtv%c
   else if ( size(v_list) == 2 ) then
      write ( fmt, * ) '(DT"data"(', v_list(1),'),A', v_list(2) ,')'
      read ( unit, fmt, iostat = iostat, iomsg = iomsg )          dtv%d, dtv%c
      if ( ( iostat /= 0 )  .or. ( iomsg /= 'dataread' )  ) error stop 7_4
   end if

   iomsg = 'dtioread'

end subroutine

subroutine readformatteddata (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: data, rbuffer, idx

   type(data), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(4) :: fmt

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1
   write ( fmt, "(A2,I1,A1)" )  '(I', v_list(1),')'
   read ( unit, fmt, iostat = iostat )      dtv%i

   iomsg = 'dataread'

end subroutine


