!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar107kl
!*
!*  DATE                       : 2007-06-06 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        scalar sequence derived type variable
!*                                        containing sequence components which uses DTIO (read)
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

   type data (kd)
      integer, kind :: kd
      sequence
      integer(kd) :: i
   end type

   type base (kb,lb)
      integer, kind :: kb
      integer, len :: lb
      sequence
      type(data(kb))   :: d
      character(lb) :: c
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         type(base(4,*)), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface
   integer :: idx
   character(20) :: rbuffer(6)
end module

program scalar107kl
use m

   type(base(4,:)), allocatable :: b1
   type(base(4,:)), pointer     :: b2
   type(base(4,3))              :: b3 = base(4,3)(data(4)(-999),'xxx')

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT(5))"

   open (1, file = 'scalar107kl.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(4,3)(data(4)(-999),'xxx') )
   allocate ( b2, source = base(4,3)(data(4)(-999),'xxx') )

10 format (DT'b2'(6,7))
   idx = 1
   rbuffer = ''

   read ( 1, fmt, iostat = stat, iomsg = msg )               b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read ( 1, 10, iostat = stat, iomsg = msg )                b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   read ( 1, "(DT'b3'(8,9))", iostat = stat, iomsg = msg )    b3
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   print *, rbuffer

   if ( ( b1%d%i /= 100 ) .or. ( b1%c /= 'abc' ) )    error stop 4_4
   if ( ( b2%d%i /= 200 ) .or. ( b2%c /= 'def' ) )    error stop 5_4
   if ( ( b3%d%i /= 300 ) .or. ( b3%c /= 'ghi' ) )    error stop 6_4

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, data, rbuffer, idx

   interface read(formatted)
      subroutine readformatteddata(dtv, unit, iotype, v_list, iostat, iomsg )
         import data
         type(data(4)), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   type(base(4,*)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt
   write ( rbuffer(idx), * ) iotype, v_list
   iomsg = ''
   idx = idx + 1

   if ( size(v_list) == 1 ) then
      write ( fmt, * ) '(I', v_list(1),', 1X,A3 )'
      read ( unit, fmt, iostat = iostat, iomsg = iomsg )    dtv%d%i, dtv%c
      if ( ( iostat /= 0 )  .or. ( iomsg /= '' )  ) error stop 7_4
   else if ( size(v_list) == 2 ) then
      write ( fmt, * ) '(DT"data"(', v_list(1),'),1X,A', v_list(2) ,')'
      read ( unit, fmt, iostat = iostat, iomsg = iomsg )          dtv%d, dtv%c
      if ( ( iostat /= 0 )  .or. ( iomsg /= 'dataread' )  ) error stop 8_4
   end if

   iomsg = 'dtioread'

end subroutine

subroutine readformatteddata (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: data, rbuffer, idx

   type(data(4)), intent(inout) :: dtv
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


