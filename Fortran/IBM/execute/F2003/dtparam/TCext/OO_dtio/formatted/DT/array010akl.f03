!*  ===================================================================
!*
!*  DATE                       : 2007-06-04 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        io-implied-do with scalar (non-)polymorphic
!*                                        derived type array variable with polymorphic component
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

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program array010akl
use m

   class(base(4)), allocatable :: b1(:)
   type(base(4)), pointer      :: b2(:)
   type(base(4))               :: b3(3)

   integer :: stat
   character(150) :: msg
   character(32) :: fmt = "(DT'_b1'(5,6,7),/,DT'_b2'(5,10))"

   open (1, file = 'array010akl.1', form='formatted', access='sequential' )

   allocate ( b1(3:5), source = (/ base(4)(cdata(4,4)(101,111), 1001), base(4)(cdata(4,4)(102, 112), 1002), base(4)(cdata(4,4)(103,113), 1003) /) )
   allocate ( b2(2:4), source = (/ base(4)(data(4)(201), 2001), base(4)(data(4)(202), 2002), base(4)(data(4)(203), 2003) /) )
   b3 = (/ base(4)(d=cdata(4,4)(301, 311), k= 3001), base(4)(d=data(4)(302), k= 3002) , base(4)(d=cdata(4,4)(303, 313), k= 3003)  /)

   write ( 1, fmt, iostat = stat, iomsg = msg )                 ( b1(i-1), b2(i-2), i= 4,6 )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, "( DT'_b3-1'(5,6,7),/, DT'_b3-2'(5,10))", iostat = stat, iomsg = msg )   ( b3(i-2), i = 3,5)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, data

   interface write(formatted)
      subroutine writeformatteddata(dtv, unit, iotype, v_list, iostat, iomsg )
         import data
         class(data(4)), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(60) :: fmt

   write ( fmt, * ) "( A, I4, I", v_list(1), ", DT'data'(", (v_list(i), "," ,i=2,size(v_list)-1), v_list(size(v_list)),") )"
   write ( unit, fmt, iomsg = iomsg ) iotype, v_list(1), dtv%k, dtv%d

   if ( iomsg /= 'datawrite' ) error stop 4_4

   iomsg = 'dtiowrite'

end subroutine

subroutine writeformatteddata (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: data, cdata

   class(data(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(40) :: fmt = ''

   select type ( dtv )
      type is (data(4))
         write ( fmt, * ) "( A, I4, I", v_list(1), ")"
         write ( unit, fmt ) iotype, v_list, dtv%i
      type is (cdata(4,4))
         write ( fmt, * ) "( A, I4, I4, I", v_list(1), ", I", v_list(2), ")"
         write ( unit, fmt ) iotype, v_list, dtv%i, dtv%j
   end select

   iomsg = 'datawrite'

end subroutine


