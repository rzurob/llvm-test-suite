! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10.1.2 Namelist Input Values
!*                                        When a slash is encountered, input statement should terminate.
!*                                        Inside dtio, and try to use X editor to skip over the slash
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
      integer(4)   :: i(3) = (/ -9, -9, -9 /)
   end type

end module

program slash001
   use m

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

   integer :: stat
   character(150) :: msg = ''

   class(base), allocatable  :: b1
   class(base), pointer      :: b2
   type(base)                :: b3
   type(base), pointer       :: b4

   namelist /n1/ b1, b3
   namelist /n2/ b2, b4

   allocate( b1, b2,  b4)

   open (1, file='slash001.1', form='formatted', access='sequential', blank='zero' )

   read (1, n1, iostat = stat, iomsg = msg)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read (1, n2, iostat = stat, iomsg = msg)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   if ( ( b1%i(1) /= 1234 ) .or. ( b1%i(2) /= 5678 ) .or. ( b1%i(3) /= 9012 ) ) error stop 3_4
   if ( ( b2%i(1) /= 1234 ) .or. ( b2%i(2) /= 5678 ) .or. ( b2%i(3) /= 9012 ) ) error stop 4_4
   if ( ( b3%i(1) /= 2345 ) .or. ( b3%i(2) /= 6789 ) .or. ( b3%i(3) /= 1234 ) ) error stop 5_4
   if ( ( b4%i(1) /= 2345 ) .or. ( b4%i(2) /= 6789 ) .or. ( b4%i(3) /= 1234 ) ) error stop 6_4


end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= 'NAMELIST' ) error stop 7_4
   if ( size(v_list,1) /= 0 )  error stop 8_4

   read( unit, "(I4,1X,I4,1X,I4)", iostat = iostat) dtv%i

   iomsg = 'dtioread'

end subroutine