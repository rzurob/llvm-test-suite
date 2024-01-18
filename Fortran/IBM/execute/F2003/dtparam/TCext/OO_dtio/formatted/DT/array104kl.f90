!*  ===================================================================
!*
!*  TEST CASE NAME             : array104kl
!*
!*  DATE                       : 2007-06-04 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        array (non-)polymorphic derived type variable with polymorphic component (array)
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
      integer(kd) :: i = -9
   end type

   type, extends(data) :: cdata (kc)
      integer, kind :: kc
      integer(kc) :: j = -9
   end type

   type base (kb)
      integer, kind :: kb
      class(data(kb)), allocatable :: d
      integer(kb) :: k = -9
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
   character(15) :: rbuffer(22)

   contains
      subroutine printdata(dtv)
         class(base(4)), intent(in) :: dtv
         select type ( g => dtv%d )
            type is (data(4))
               print *, dtv%k, g%i
            type is (cdata(4,4))
               print *, dtv%k, g%i, g%j
         end select
      end subroutine
end module

program array104kl
use m

   class(base(4)), allocatable :: b1(:)
   type(base(4)), pointer      :: b2(:)
   type(base(4))               :: b3(4)

   integer :: stat
   character(150) :: msg
   character(53) :: fmt = "(DT'b1-1'(5,6,7),/,DT'b1-2'(5,7,8),/,DT'b1-3'(5,8,9))"

   open (1, file = 'array104kl.1', form='formatted', access='sequential' )

   allocate ( b1(5), source = (/base(4)(cdata(4,4)(-9,-9), -9), base(4)(d=cdata(4,4)()), base(4)(cdata(4,4)(-9,-9), -9), base(4)(d=cdata(4,4)()), base(4)(cdata(4,4)(-9,-9), -9) /))

   allocate ( b2(4), source = (/ base(4)(data(4)(-9), -9), base(4)(cdata(4,4)(-9, -9), -9), &
                                 base(4)(data(4)(-9), -9), base(4)(cdata(4,4)(-9, -9), -9) /) )

   b3 = (/ base(4)(d=cdata(4,4)(-9, -9), k= -9), base(4)(d=cdata(4,4)(-9, -9), k= -9), &
           base(4)(d=cdata(4,4)(-9, -9), k= -9), base(4)(d=cdata(4,4)(-9, -9), k= -9) /)


10 format (DT'b2-1'(5,6), TR1, DT'b2-2'(5,6,7),/, DT'b2-3'(5,6), 1X, DT'b2-4'(5,6,7) )

   idx = 1

   read ( 1, fmt, iostat = stat, iomsg = msg )                 b1(1:5:2)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read ( 1, 10, iostat = stat, iomsg = msg )                  b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   read ( 1, "(DT'b3'(7,8,9))", iostat = stat, iomsg = msg )   b3(4:1:-1)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   call printdata(b1(1))
   call printdata(b1(2))
   call printdata(b1(3))
   call printdata(b1(4))
   call printdata(b1(5))

   call printdata(b2(1))
   call printdata(b2(2))
   call printdata(b2(3))
   call printdata(b2(4))

   call printdata(b3(1))
   call printdata(b3(2))
   call printdata(b3(3))
   call printdata(b3(4))

   print *, rbuffer

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, data, rbuffer, idx

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

   character(60) :: fmt

   write ( rbuffer(idx), * ) iotype, v_list
   idx = idx + 1

   write ( fmt, * ) "(  I", v_list(1), ",1X, DT'data'(", (v_list(i), "," ,i=2,size(v_list)-1), v_list(size(v_list)),") )"
   read ( unit, fmt, iomsg = iomsg ) dtv%k, dtv%d

   if ( iomsg /= 'dataread' ) error stop 4_4

   iomsg = 'dtioread'

end subroutine

subroutine readformatteddata (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: data, cdata, rbuffer, idx

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


