!*  ===================================================================
!*
!*  DATE                       : 2007-06-06 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        scalar (non-)polymorphic derived type variable with polymorphic component (read)
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

   type, extends(data) :: cdata (kc)
      integer, kind :: kc
      integer(kc) :: j
   end type

   type base (kb)
      integer, kind :: kb
      class(data(kb)), allocatable :: d
      integer(kb) :: k
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
   character(20) :: rbuffer(6)

end module

program scalar104kl
use m

   class(base(4)), allocatable :: b1
   type(base(4)), pointer      :: b2
   type(base(4))               :: b3

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT'b1'(5,6,7))"

   open (1, file = 'scalar104kl.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(4)(cdata(4,4)(-999,-999), -999) )
   allocate ( b2, source = base(4)(data(4)(-999), -999) )
   b3 = base(4)(d=cdata(4,4)(-999, -999), k= -999)

10 format (DT'b2'(6,7) )

   idx = 1

   read ( 1, fmt, iostat = stat, iomsg = msg )                 b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read ( 1, 10, iostat = stat, iomsg = msg )                  b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   read ( 1, "(DT'b3'(7,8,9))", iostat = stat, iomsg = msg )   b3
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   print *, rbuffer

   select type ( g => b1%d )
      type is ( cdata(4,4) )
         if ( ( g%i /= 101 ) .or. ( g%j /= 102 ) .or. ( b1%k /= 1001 ) )  error stop 4_4
   end select

   if ( ( b2%d%i /= 103 ) .or. ( b2%k /= 1002 ) )                         error stop 5_4

   select type ( g => b3%d )
      type is ( cdata(4,4) )
         if ( ( g%i /= 104 ) .or. ( g%j /= 105 ) .or. ( b3%k /= 1003 ) )  error stop 6_4
   end select

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, data, idx, rbuffer

   interface read(formatted)
      subroutine readformatteddata(dtv, unit, iotype, v_list, iostat, iomsg )
         import data
         class(data(4)), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base(4)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(30) :: fmt

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   write ( fmt, * ) "( I", v_list(1), ", DT'data'(", (v_list(i), "," ,i=2,size(v_list)-1), v_list(size(v_list)),") )"
   read ( unit, fmt, iomsg = iomsg ) dtv%k, dtv%d

   if ( iomsg /= 'dataread' ) error stop 4_4

   iomsg = 'dtioread'

end subroutine

subroutine readformatteddata (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: data, cdata, idx, rbuffer

   class(data(4)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(40) :: fmt = ''

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1
   select type ( dtv )
      type is (data(4))
         write ( fmt, * ) "( I", v_list(1), ")"
         read ( unit, fmt ) dtv%i
      type is (cdata(4,4))
         write ( fmt, * ) "( I", v_list(1), ", I", v_list(2), ")"
         read ( unit, fmt ) dtv%i, dtv%j
   end select

   iomsg = 'dataread'

end subroutine

