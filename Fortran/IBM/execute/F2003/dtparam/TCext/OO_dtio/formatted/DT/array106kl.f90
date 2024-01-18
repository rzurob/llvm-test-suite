!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : array106kl
!*
!*  PROGRAMMER                 : David Forster (derived from array106 by Robert Ma)
!*  DATE                       : 2007-06-04 (original: 21/03/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        array sequence derived type variable containing sequence components (read)
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
   character(15) :: rbuffer(9)
end module

program array106kl
use m

   type(base(4,3)), allocatable, target :: b1(:)
   type(base(4,3)), pointer     :: b2(:)
   type(base(4,3))              :: b3(3)

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT'b1-1'(5),DT'b1-2'(6))"

   open (1, file = 'array106kl.1', form='formatted', access='sequential' )

   allocate ( b1(4) )
   b2 => b1(1:3:2)

10 format (DT'b2-1',/,DT'b2-2'(7))

   idx = 1

   read ( 1, fmt, iostat = stat, iomsg = msg )               b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   if ( ( b1(1)%d%i  /= 100 ) .or. ( b1(2)%d%i  /= 101 ) .or. ( b1(3)%d%i  /= 102 ) .or. ( b1(4)%d%i  /= 103 ) .or.  &
        ( b1(1)%c  /= 'abc' ) .or. ( b1(2)%c  /= 'def' ) .or. ( b1(3)%c  /= 'ghi' ) .or. ( b1(4)%c  /= 'jkl' ) ) error stop 2_4

   read ( 1, 10, iostat = stat, iomsg = msg )                b2(2:1:-1)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   if ( ( b2(1)%d%i  /= 200 ) .or. ( b2(2)%d%i  /= 201 ) .or.  &
        ( b2(1)%c  /= 'ABC' ) .or. ( b2(2)%c  /= 'DEF' ) ) error stop 2_4

   if ( ( b1(1)%d%i  /= 200 ) .or. ( b1(2)%d%i  /= 101 ) .or. ( b1(3)%d%i  /= 201 ) .or. ( b1(4)%d%i  /= 103 ) .or.  &
        ( b1(1)%c  /= 'ABC' ) .or. ( b1(2)%c  /= 'def' ) .or. ( b1(3)%c  /= 'DEF' ) .or. ( b1(4)%c  /= 'jkl' ) ) error stop 2_4

   read ( 1, "(DT'b3'(10))", iostat = stat, iomsg = msg )    b3
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 4_4

   if ( ( b3(1)%d%i  /= 300 ) .or. ( b3(2)%d%i  /= 301 ) .or. ( b3(3)%d%i  /= 302 ) .or.  &
        ( b3(1)%c  /= 'abc' ) .or. ( b3(2)%c  /= 'def' ) .or. ( b3(3)%c  /= 'ghi' ) ) error stop 2_4

   print *, rbuffer

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
      read ( unit, fmt, iostat = iostat )          dtv%d, dtv%c
   else
      read ( unit, "(I4,1X,A3)", iostat = iostat ) dtv%d, dtv%c
   end if

   iomsg = 'dtioread'

end subroutine
