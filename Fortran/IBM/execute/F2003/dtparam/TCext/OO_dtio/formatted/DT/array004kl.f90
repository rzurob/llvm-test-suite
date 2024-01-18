!*  ===================================================================
!*
!*  DATE                       : 2007-06-04 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        array (non-)polymorphic derived type variable with polymorphic component
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

program array004kl
use m

   class(base(4)), allocatable :: b1(:)
   type(base(4)), pointer      :: b2(:)
   type(base(4))               :: b3(4)

   integer :: stat
   character(150) :: msg
   character(53) :: fmt = "(DT'b1-1'(5,6,7),/,DT'b1-2'(5,7,8),/,DT'b1-3'(5,8,9))"

   open (1, file = 'array004kl.1', form='formatted', access='sequential' )

   allocate ( b1(5), source = (/base(4)(cdata(4,4)(111,112), 1001), base(4)(d=null()), base(4)(cdata(4,4)(121,122), 1002), base(4)(d=null()), base(4)(cdata(4,4)(131,132), 1003) /))

   allocate ( b2(4), source = (/ base(4)(data(4)(211), 2001), base(4)(cdata(4,4)(221, 222), 2002), &
                                 base(4)(data(4)(231), 2002), base(4)(cdata(4,4)(241, 242), 2004) /) )

   b3 = (/ base(4)(d=cdata(4,4)(311, 312), k= 3001), base(4)(d=cdata(4,4)(321, 322), k= 3002), &
           base(4)(d=cdata(4,4)(331, 332), k= 3003), base(4)(d=cdata(4,4)(341, 342), k= 3004) /)

10 format (DT'b2-1'(5,6), TR1, DT'b2-2'(5,6,7),/, DT'b2-3'(5,6), 1X, DT'b2-4'(5,6,7) )

   write ( 1, fmt, iostat = stat, iomsg = msg )                 b1(1:5:2)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, 10, iostat = stat, iomsg = msg )                  b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   write ( 1, "(DT'b3'(7,8,9))", iostat = stat, iomsg = msg )   b3((/4,2,3,1/))
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

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

   write ( fmt, * ) "( A, I4, I", v_list(1), ",1X, DT'data'(", (v_list(i), "," ,i=2,size(v_list)-1), v_list(size(v_list)),") )"
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


