!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Array unlimited polymorphic entity contains
!*                                        unlimited polymorphic component which invokes another dtio
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

   type data
      integer(4) :: i
   end type

   type, extends(data) :: cdata
      integer(4) :: j
   end type

   type base
      class(*), allocatable :: d
      integer(4) :: k
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program array009a
use m

   class(*), allocatable :: u1(:)
   class(*), pointer     :: u2(:,:)

   integer :: stat
   character(150) :: msg
   character(33) :: fmt = "(DT'u1-1'(5,6,7),/,DT'u1-2'(5,9))"

   open (1, file = 'array009a.1', form='formatted', access='sequential' )

   allocate ( u1(3), source = (/ base(cdata(101,111), 1001), base(data(102), 1002), base(cdata(103,113), 1003) /))
   select type ( u1 )
      type is ( base )
         allocate ( u2(2,2), source = reshape ( source = (/ base(data(104), 1004), u1(3:1:-1) /), shape = (/2,2/) ) )
   end select

10 format (2(DT'u2-1'(5,9),/,DT'u2-2'(5,5,10),/))

   select type ( u1 )
      class is ( base )
         write ( 1, fmt, iostat = stat, iomsg = msg )                 u1
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
   end select

   select type ( u2 )
      class is ( base )
         write ( 1, 10, iostat = stat, iomsg = msg )                  u2
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
   end select

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, data

   interface write(formatted)
      subroutine writeformatteddata(dtv, unit, iotype, v_list, iostat, iomsg )
         import data
         class(data), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(60) :: fmt

   write ( fmt, * ) "( A, I4, I", v_list(1), ", DT'data'(", (v_list(i), "," ,i=2,size(v_list)-1), v_list(size(v_list)),") )"
   select type ( g => dtv%d )
      class is ( data )
         write ( unit, fmt, iomsg = iomsg ) iotype, v_list(1), dtv%k, g
   end select

   if ( iomsg /= 'datawrite' ) error stop 4_4

   iomsg = 'dtiowrite'

end subroutine

subroutine writeformatteddata (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: data, cdata

   class(data), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(40) :: fmt = ''

   select type ( dtv )
      type is (data)
         write ( fmt, * ) "( A, I4, I", v_list(1), ")"
         write ( unit, fmt ) iotype, v_list, dtv%i
      type is (cdata)
         write ( fmt, * ) "( A, I4, I4, I", v_list(1), ", I", v_list(2), ")"
         write ( unit, fmt ) iotype, v_list, dtv%i, dtv%j
   end select

   iomsg = 'datawrite'

end subroutine

