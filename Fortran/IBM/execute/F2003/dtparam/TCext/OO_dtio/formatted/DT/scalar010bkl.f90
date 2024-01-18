!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar010bkl
!*
!*  DATE                       : 2007-06-05 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        io-implied-do inside DTIO for derived type components
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
      type(data(kb)), allocatable :: d(:)
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

program scalar010bkl
use m

   class(base(4)), allocatable :: b1
   type(base(4)), pointer      :: b2
   type(base(4))               :: b3

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT'_b1')"

   open (1, file = 'scalar010bkl.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(4)((/ data(4)(100), data(4)(101) /) , 1001) )
   allocate ( b2, source = base(4)((/ data(4)(200), data(4)(201) /) , 2002) )
   b3 = base(4)((/ data(4)(300), data(4)(301), data(4)(302)         /) , 3003)

   write ( 1, fmt, iostat = stat, iomsg = msg )                 b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, *, iostat = stat, iomsg = msg )                   b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   write ( 1, '(DT"_b3"(1))', iostat = stat, iomsg = msg )      b3
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

   write ( unit, * ) iotype, v_list
   write ( fmt, * ) '(', ( 'DT(',i,'),',i=1,size(dtv%d)-1 ) ,' DT(', size(dtv%d), '), 1X, I4)'
   write ( unit, fmt, iomsg = iomsg ) (dtv%d(i),i=1,size(dtv%d)), dtv%k

   if ( iomsg /= 'datawrite' ) error stop 4_4

   iomsg = 'dtiowrite'

end subroutine

subroutine writeformatteddata (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: data

   class(data(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(40) :: fmt = ''

   write ( unit, "(/)" )
   write ( unit, * ) 'iotype: ', iotype, ' v_list: ', v_list
   write ( unit, "(I4)" )  dtv%i

   iomsg = 'datawrite'

end subroutine


