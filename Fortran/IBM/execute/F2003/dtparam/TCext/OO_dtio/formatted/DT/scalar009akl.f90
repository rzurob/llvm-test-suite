!*  ===================================================================
!*
!*  DATE                       : 2007-06-05 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        scalar (non-)polymorphic derived type variable with polymorphic component
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
      class(*), allocatable :: d
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

program scalar009akl
use m

   class(*), allocatable :: u1
   class(*), pointer     :: u2

   integer :: stat
   character(150) :: msg
   character(30) :: fmt = "(DT'u1'(5,6,7))"

   open (1, file = 'scalar009akl.1', form='formatted', access='sequential' )

   allocate ( u1, source = base(4)(cdata(4,4)(101,102), 1001) )
   allocate ( u2, source = base(4)(data(4)(103), 1002) )

10 format (DT'u2'(5,10) )

   select type ( u1 )
      class is ( base(4) )
         write ( 1, fmt, iostat = stat, iomsg = msg )                 u1
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
   end select

   select type ( u2 )
      class is ( base(4) )
         write ( 1, 10, iostat = stat, iomsg = msg )                 u2
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
   end select

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
   select type ( g => dtv%d )
      class is ( data(4) )
         write ( unit, fmt, iomsg = iomsg ) iotype, v_list(1), dtv%k, g
   end select

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


