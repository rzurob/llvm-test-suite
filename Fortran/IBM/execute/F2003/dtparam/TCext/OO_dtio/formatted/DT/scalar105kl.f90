!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar105kl
!*
!*  DATE                       : 2007-06-06 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        scalar sequence derived type variable (read)
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

   type base (kb,lb)
      integer, kind :: kb
      integer, len :: lb
      sequence
      integer(kb)   :: i
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
   character(20) :: rbuffer(3)
end module

program scalar105kl
use m

   type(base(4,:)), allocatable :: b1
   type(base(4,:)), pointer     :: b2
   type(base(4,3))              :: b3 = base(4,3)(-999,'xxx')

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT)"

   open (1, file = 'scalar105kl.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(4,3)(-999,'xxx') )
   allocate ( b2, source = base(4,3)(-999,'xxx') )

10 format (DT'b2')

   idx = 1

   read ( 1, fmt, iostat = stat, iomsg = msg )               b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read ( 1, 10, iostat = stat, iomsg = msg )                b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   read ( 1, "(DT'b3'(10))", iostat = stat, iomsg = msg )    b3
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   print *, rbuffer

   if ( ( b1%i /= 100 ) .or. ( b1%c /= 'abc' ) )    error stop 4_4
   if ( ( b2%i /= 200 ) .or. ( b2%c /= 'def' ) )    error stop 5_4
   if ( ( b3%i /= 300 ) .or. ( b3%c /= 'ghi' ) )    error stop 6_4

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, rbuffer, idx

   type(base(4,*)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(20) :: fmt

   write ( rbuffer(idx), * ) iotype, v_list

   idx = idx + 1

   if ( size(v_list) /= 0 ) then
      write ( fmt, * ) '(I', v_list(1),', 1X,A3 )'
      read ( unit, fmt, iostat = iostat )    dtv%i, dtv%c
   else
      read ( unit, "(I4,1X,A3)", iostat = iostat ) dtv%i, dtv%c
   end if

   iomsg = 'dtioread'

end subroutine
