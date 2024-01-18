! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10.1.3 Namelist group object list items
!*                                        try end of record in the middle of character types
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
      character(3)   :: c(3) = (/ 'xxx', 'xxx', 'xxx' /)
   end type
   type nodtio
      character(3)   :: c(3) = (/ 'xxx', 'xxx', 'xxx' /)
   end type
end module

program character001
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
   character(4)              :: c(2)
   class(base), pointer      :: b3
   type(nodtio), allocatable :: b4

   namelist /n1/ b1, b2, c
   namelist /n2/ b3, b4

   allocate (b1, b2, b3, b4)

   open (1, file='character001.1', form='formatted', access='sequential' )

   read (1, n1, iostat = stat, iomsg = msg)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
   read (1, n2, iostat = stat, iomsg = msg)

   if ( ( b1%c(1) /= 'abc' )  .or. ( b1%c(2) /= 'def' ) .or. ( b1%c(3) /= '/gh' ) ) error stop 1_4
   if ( ( b2%c(1) /= 'abc' )  .or. ( b2%c(2) /= 'd  ' ) .or. ( b2%c(3) /= 'gh ' ) ) error stop 2_4
   if ( ( c(1) /= 'ibm1' ) .or. ( c(2) /= 'fort' ) ) error stop 3_4
   if ( ( b3%c(1) /= 'IBM' )  .or. ( b3%c(2) /= 'xxx' ) .or. ( b3%c(3) /= '   ' ) ) error stop 4_4
   if ( ( b4%c(1) /= '   ' )  .or. ( b4%c(2) /= '   ' ) .or. ( b4%c(3) /= 'abc' ) ) error stop 5_4

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
