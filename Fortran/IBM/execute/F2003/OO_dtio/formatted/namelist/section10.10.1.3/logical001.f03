! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10.1.3 Namelist group object list items
!*                                        try logical with namelist formatting
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
      logical :: true(3) = (/ .true.  , .true., .true.  /)
      logical :: false(3)= (/ .false. , .false., .false. /)
   end type
end module

program logical001
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

   namelist /n1/ b1, b2

   allocate (b1, b2)

   open (1, file='logical001.1', form='formatted', access='sequential' )

   read (1, n1, iostat = stat, iomsg = msg)

   print *,b1%true
   print *,b1%false
   print *,b2%true
   print *,b2%false

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   logical :: true(3), false(3)

   namelist /dtio/ true

   if ( iotype /= 'NAMELIST' ) error stop 1_4
   if ( size(v_list,1) /= 0 )  error stop 2_4

   read( unit, dtio, iostat = iostat)
   false = (.not. true)
   dtv%true = true
   dtv%false = false

   iomsg = 'dtioread'

end subroutine
