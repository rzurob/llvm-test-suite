!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar010akl
!*
!*  DATE                       : 2007-06-05 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        io-implied-do with scalar (non-)polymorphic
!*                                        derived type variable with polymorphic component
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

program scalar010akl
use m

   class(base(4)), allocatable :: b1
   type(base(4)), pointer      :: b2
   type(base(4))               :: b3

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT'_b1'(5,6,7),DT'_b2'(6,7))"

   open (1, file = 'scalar010akl.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(4)(cdata(4,4)(101,102), 1001) )
   allocate ( b2, source = base(4)(data(4)(103), 1002) )
   b3 = base(4)(d=cdata(4,4)(104, 105), k= 1003)

   write ( 1, fmt, iostat = stat, iomsg = msg )                 ( b1, b2, i= 4,5 )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, "('B3-1: ', DT'_b3-1'(5,6,7),:,1X,'B3-2:', DT'_b3-2'(6,7,8))", iostat = stat, iomsg = msg )   ( b3, i = 3,5)
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


