! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10.1.3 Namelist group object list items
!*                                        if not enough characters are supplied by input data,
!*                                        the remaining date shall be filled with blanks
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
      character(7)   :: c(3) = (/ 'xxxxxxx', 'xxxxxxx', 'xxxxxxx' /)
   end type

   type nodtio
      character(7)   :: c(3) = (/ 'xxxxxxx', 'xxxxxxx', 'xxxxxxx' /)
   end type

end module

program character002
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
   type(nodtio), pointer     :: b2
   class(base), pointer      :: b3
   type(nodtio), allocatable :: b4

   namelist /n1/ b1, b2
   namelist /n2/ b3, b4

   allocate (b1, b2, b3, b4)

   open (1, file='character002.1', form='formatted', access='sequential' )

   read (1, n1, iostat = stat, iomsg = msg)
   read (1, n2, iostat = stat, iomsg = msg)

   if ( ( b1%c(1) /= 'abcdefg' )  .or. ( b1%c(2) /= 'ABCDE  ' ) .or. ( b1%c(3) /= 'xxxxxxx' ) ) error stop 1_4
   if ( ( b2%c(1) /= 'ABCDEFG' )  .or. ( b2%c(2) /= 'abc    ' ) .or. ( b2%c(3) /= 'xxxxxxx' ) ) error stop 2_4
   if ( ( b3%c(1) /= 'abcdefg' )  .or. ( b3%c(2) /= 'ABCDE  ' ) .or. ( b3%c(3) /= 'xxxxxxx' ) ) error stop 3_4
   if ( ( b4%c(1) /= 'ABCDEFG' )  .or. ( b4%c(2) /= 'abc    ' ) .or. ( b4%c(3) /= 'xxxxxxx' ) ) error stop 4_4
end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   type(base)           :: dummy
   namelist /dtio/ dummy

   if ( iotype /= 'NAMELIST' ) error stop 3_4
   if ( size(v_list,1) /= 0 )  error stop 4_4

   read( unit, dtio, iostat = iostat)

   dtv%c = dummy%c

   iomsg = 'dtioread'

end subroutine
