!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        v-list being signed-int ( -ve/+ve/no-sign )
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
   type base
      integer(4) :: i
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

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

program fdtedit105
use m

   class(base), allocatable :: b1
   class(base), pointer     :: b2

   integer :: stat
   character(150) :: msg
   character(30) :: format = "(DT'read'(-7,+8,9,-10,+11,12))"
   open (1, file = 'fdtedit105.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(100) )
   allocate ( b2, source = base(200) )

10 format (DT'write'(-1,+2,3,-4,+5,6))

   write ( 1, 10, iostat = stat, iomsg = msg )   b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   rewind 1

   read ( 1,  fmt=format, iostat = stat, iomsg = msg )   b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )  error stop 2_4
   if ( b2%i /= 100 )                               error stop 3_4

   close ( 1, status='delete' )

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "DTread" ) error stop 4_4
   if ( ( size(v_list, 1) /= 6 ) .or. ( v_list(1) /= -7 ) .or. ( v_list(2) /= 8 ) .or. ( v_list(3) /= 9 ) .or. &
        ( v_list(4) /= -10 ) .or. ( v_list(5) /= 11 ) .or. ( v_list(6) /= 12 ) .or. ( len(iotype) /= 6 ) ) error stop 5_4

   read ( unit, "(I4)", iostat = iostat ) dtv%i
   iomsg = 'dtioread'

end subroutine

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "DTwrite" ) error stop 6_4
   if ( ( size(v_list, 1) /= 6 ) .or. ( v_list(1) /= -1 ) .or. ( v_list(2) /= 2 ) .or. ( v_list(3) /= 3 ) .or. &
        ( v_list(4) /= -4 ) .or. ( v_list(5) /= 5 ) .or. ( v_list(6) /= 6 ) .or. ( len(iotype) /= 7 ) ) error stop 7_4

   write ( unit, "(I4)", iostat = iostat ) dtv%i
   iomsg = 'dtiowrite'

end subroutine
