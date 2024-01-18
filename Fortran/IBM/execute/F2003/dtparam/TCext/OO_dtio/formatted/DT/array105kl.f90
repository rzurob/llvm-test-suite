!*  ===================================================================
!*
!*  DATE                       : 2007-06-04 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        array sequence derived type variable (array)
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
      integer(kb)   :: i = -9
      character(lb) :: c = 'xxx'
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
   character(15) :: rbuffer(10)

end module

program array105kl
use m

   type(base(4,3)), allocatable :: b1(:)
   type(base(4,3)), pointer     :: b2(:,:)
   type(base(4,3))              :: b3(3)

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT,/,DT,/,DT'_b1'(4))"

   open (1, file = 'array105kl.1', form='formatted', access='sequential' )

   allocate ( b1(3) )
   allocate ( b2(2,2) )

10 format (DT'b2-1'(-5),/,DT'b2-2'(-6),/,DT'b2-3'(-7),/,DT'b2-4'(-8))

   idx = 1
   read ( 1, fmt, iostat = stat, iomsg = msg )               b1((/3,1,2/))
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read ( 1, 10, iostat = stat, iomsg = msg )                b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   read ( 1, "(DT'b3-1'(10),/,DT'b3-2'(-10))", iostat = stat, iomsg = msg )    b3
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   if ( ( b1(1)%i  /= 100 ) .or. ( b1(2)%i  /= 101 ) .or. ( b1(3)%i  /= 102 ) .or.  &
        ( b1(1)%c  /= 'abc' ) .or. ( b1(2)%c  /= 'def' ) .or. ( b1(3)%c  /= 'ghi' ) ) error stop 4_4

   if ( ( b2(1,1)%i  /= 200 )   .or. ( b2(2,1)%i  /= 201 )   .or. ( b2(1,2)%i  /= 202 )  .or. ( b2(2,2)%i  /= 203 ) .or. &
        ( b2(1,1)%c  /= 'ABC' ) .or. ( b2(2,1)%c  /= 'DEF' ) .or. ( b2(1,2)%c  /= 'GHI' ).or. ( b2(2,2)%c  /= 'JKL' )  ) error stop 5_4

   if ( ( b3(1)%i  /= 300 ) .or. ( b3(2)%i  /= 301 ) .or. ( b3(3)%i  /= 302 ) .or.  &
        ( b3(1)%c  /= 'abc' ) .or. ( b3(2)%c  /= 'def' ) .or. ( b3(3)%c  /= 'ghi' ) ) error stop 6_4

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

   character(40) :: fmt

   write ( rbuffer(idx), * ) iotype, v_list

   idx = idx + 1
   if ( size(v_list) /= 0 ) then
      if ( v_list(1) .lt. 0 ) then
      	 write ( fmt, * ) '(I', -1*v_list(1),', 1X,A3 )'
      else
         write ( fmt, * ) '(I', v_list(1),', 1X,A3 )'
      end if
      read ( unit, fmt, iostat = iostat )    dtv%i, dtv%c
   else
      read ( unit, "(I4,1X,A3)", iostat = iostat ) dtv%i, dtv%c
   end if

   iomsg = 'dtioread'

end subroutine
