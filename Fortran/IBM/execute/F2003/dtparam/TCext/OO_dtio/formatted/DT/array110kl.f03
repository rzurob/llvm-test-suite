!*  ===================================================================
!*
!*  DATE                       : 2007-06-05 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        io-implied-do with array derived type variable (read)
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
      integer(kb) :: i
   end type

   type, extends( base ) :: child (kc)
      integer, kind :: kc
      integer(kc) :: j
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
   end interface

   integer :: idx
   character(15) :: rbuffer(17)

end module

program array110kl
use m

   class(base(4)), allocatable :: b1(:)
   type(base(4)), pointer      :: b2(:,:)
   type(child(4,4))              :: b3(6:9)

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT(-1),/, DT(-2),/, DT(-3))"

   open (1, file = 'array110kl.1', form='formatted', access='sequential' )

   allocate ( b1(3) )
   allocate ( b2(2,2) )

10 format (DT'b2-1'(-2,-4),/,DT'b2-2'(-6,-8))
20 format (DT'b3-1'(10),DT'b3-2'(20))

   idx = 1

   read ( 1, fmt, iostat = stat, iomsg = msg )        (b1, i = 1,3)  !<- read the array 3 times
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )    error stop 1_4
   msg = ''

   read ( 1, 10, iostat = stat, iomsg = msg )         (b2(1,i), b2(2,i), i = 1,2)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )    error stop 2_4
   msg = ''

   read ( 1, 20, iostat = stat, iomsg = msg )         (b3(i), i= 6,9)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )    error stop 3_4

   select type ( b1 )
      type is ( child(4,4) )
         if ( ( b1(1)%i /= 100 ) .or. ( b1(1)%j /= 1000 )  .or. &
              ( b1(2)%i /= 101 ) .or. ( b1(2)%j /= 1001 )  .or. &
              ( b1(3)%i /= 102 ) .or. ( b1(3)%j /= 1002 ) ) error stop 4_4
   end select

   if ( ( b2(1,1)%i /= 200 ) .or. ( b2(2,1)%i /= 201 ) .or. ( b2(1,2)%i /= 202 ) .or. ( b2(2,2)%i /= 203 ) ) error stop 5_4

   if ( ( b3(6)%i /= 300 ) .or. ( b3(7)%i /= 301 ) .or. ( b3(8)%i /= 302 ) .or. ( b3(9)%i /= 303 ) .or.  &
        ( b3(6)%j /= 3000 ) .or. ( b3(7)%j /= 3001 ) .or. ( b3(8)%j /= 3002 ) .or. ( b3(9)%j /= 3003 ) )     error stop 6_4

   print *, rbuffer

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child, rbuffer, idx

   class(base(4)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(20) :: fmt

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   select type ( dtv )
      type is (base(4))
         read ( unit, '(I4)', iostat = iostat )     dtv%i
      type is (child(4,4))
         read ( unit, '(I4,I5)', iostat = iostat )  dtv%i, dtv%j
   end select

   iomsg = 'dtioread'

end subroutine