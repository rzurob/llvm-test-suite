!*  ===================================================================
!*
!*  DATE                       : 2007-06-04 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        array (non-)polymorphic derived type variable with non-polymorphic component (read)
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
      integer(kd) :: i
   end type

   type base (kb)
      integer, kind :: kb
      type(data(kb)) :: d
      integer(kb) :: j
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
   character(15) :: rbuffer(11)

end module

program array103kl
use m

   class(base(4)), allocatable :: b1(:)
   type(base(4)), pointer      :: b2(:,:)
   type(base(4))               :: b3(3)

   integer :: stat
   character(150) :: msg
   character(31) :: fmt = "(DT'_b1-1'(5,6),DT'_b1-2'(6,7))"

   open (1, file = 'array103kl.1', form='formatted', access='sequential' )

   allocate ( b1(4) )
   allocate ( b2(2,2) )

10 format (DT'_b2-11'(7,8),DT'_b2-21'(8,9),DT'_b2-12'(7,8),DT'_b2-22'(8,9) )
   idx =1

   read ( 1, fmt, iostat = stat, iomsg = msg )               b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read ( 1, 10, iostat = stat, iomsg = msg )                b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   read ( 1, "(DT'b3-1'(9,10),:,/)", iostat = stat, iomsg = msg )   b3  !<- each element will be double spaced
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   if ( ( b1(1)%j /= 1001 )  .or. ( b1(2)%j /= 1002 )  .or. ( b1(3)%j /= 1003 )  .or. ( b1(4)%j /= 1004 ) .or. &
        ( b1(1)%d%i /= 101 ) .or. ( b1(2)%d%i /= 102 ) .or. ( b1(3)%d%i /= 103 ) .or. ( b1(4)%d%i /= 104 )  )         error stop 4_4

   if ( ( b2(1,1)%j /= 2001 )  .or. ( b2(2,1)%j /= 2002 )  .or. ( b2(1,2)%j /= 2003 )  .or. ( b2(2,2)%j /= 2004 ) .or. &
        ( b2(1,1)%d%i /= 201 ) .or. ( b2(2,1)%d%i /= 202 ) .or. ( b2(1,2)%d%i /= 203 ) .or. ( b2(2,2)%d%i /= 204 )  ) error stop 5_4

   if ( ( b3(1)%j /= 3001 )  .or. ( b3(2)%j /= 3002 )  .or. ( b3(3)%j /= 3003 ) .or. &
        ( b3(1)%d%i /= 301 ) .or. ( b3(2)%d%i /= 302 ) .or. ( b3(3)%d%i /= 303 )  )                                   error stop 6_4

   print *, rbuffer

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, data, rbuffer, idx

   class(base(4)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(30) :: fmt
   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1
   if ( size(v_list) /= 2 ) error stop 4_4

   write ( fmt, * ) "( I", v_list(1), ", I", v_list(2), ")"
   read ( unit, fmt ) dtv%d, dtv%j

   iomsg = 'dtioread'

end subroutine
