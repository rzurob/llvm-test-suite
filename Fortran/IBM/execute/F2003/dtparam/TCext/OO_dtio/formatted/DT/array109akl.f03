!*  ===================================================================
!*
!*  DATE                       : 2007-06-05 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Array unlimited polymorphic entity contains
!*                                        unlimited polymorphic component which invokes another dtio (read)
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
      integer(kd) :: i = -99
   end type

   type, extends(data) :: cdata (kc)
      integer, kind :: kc
      integer(kc) :: j = -99
   end type

   type base (kb)
      integer, kind :: kb
      class(*), allocatable :: d
      integer(kb) :: k = -99
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
   character(15) :: rbuffer(14)

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

program array109akl
use m

   class(*), allocatable :: u1(:)
   class(*), pointer     :: u2(:,:)

   integer :: stat
   character(150) :: msg
   character(33) :: fmt = "(DT'u1-1'(5,6,7),/,DT'u1-2'(5,9))"

   open (1, file = 'array109akl.1', form='formatted', access='sequential' )

   allocate ( u1(3), source = (/ base(4)(cdata(4,4)(), -9), base(4)(data(4)(), -9), base(4)(cdata(4,4)(), -9) /))
   select type ( u1 )
      type is ( base(4) )
         allocate ( u2(2,2), source = reshape ( source = (/ base(4)(data(4)(), -9), u1(3:1:-1) /), shape = (/2,2/) ) )
   end select

10 format (2(DT'u2-1'(5,9),/,DT'u2-2'(5,5,10),/))

   idx = 1

   select type ( u1 )
      class is ( base(4) )
         read ( 1, fmt, iostat = stat, iomsg = msg )                 u1
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
         call printdata( u1(1) )
         call printdata( u1(2) )
         call printdata( u1(3) )
   end select

   select type ( u2 )
      class is ( base(4) )
         read ( 1, 10, iostat = stat, iomsg = msg )                  u2
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4
         call printdata( u2(1,1) )
         call printdata( u2(2,1) )
         call printdata( u2(1,2) )
         call printdata( u2(2,2) )
   end select

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
   write ( fmt, * ) "( I", v_list(1), ", DT'data'(", (v_list(i), "," ,i=2,size(v_list)-1), v_list(size(v_list)),") )"
   select type ( g => dtv%d )
      class is ( data(4) )
         read ( unit, fmt, iomsg = iomsg ) dtv%k, g
   end select

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
         read ( unit, fmt )  dtv%i
      type is (cdata(4,4))
         write ( fmt, * ) "( I", v_list(1), ", I", v_list(2), ")"
         read ( unit, fmt )  dtv%i, dtv%j
   end select

   iomsg = 'dataread'

end subroutine

