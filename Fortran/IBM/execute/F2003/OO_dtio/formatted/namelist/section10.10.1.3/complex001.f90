! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10.1.3 Namelist group object list items
!*                                        end of record in the middle of complex numbers
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
      complex(4)   :: c(3) = (/ (-9.0,-9.0), (-9.0,-9.0), (-9.0,-9.0) /)
   end type
end module

program complex001
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
   complex(4)                :: c

   namelist /n1/ b1, b2, c

   allocate (b1, b2)

   open (1, file='complex001.1', form='formatted', access='sequential' )

   read (1, n1, iostat = stat, iomsg = msg)

   if ( ( b1%c(1) /= (1.0, 2.0) ) .or. ( b1%c(2) /= (3.0, 4.0) ) .or. ( b1%c(3) /= (5.0, 6.0) ) ) error stop 1_4
   if ( ( b2%c(1) /= (7.0, 8.0) ) .or. ( b2%c(2) /= (9.0, 4.0) ) .or. ( b2%c(3) /= (5.0, 6.0) ) ) error stop 2_4
   if ( c /= (10.0, 11.0) )  error stop 3_4

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

   if ( iotype /= 'NAMELIST' ) error stop 1_4
   if ( size(v_list,1) /= 0 )  error stop 2_4

   read( unit, dtio, iostat = iostat)

   dtv%c = dummy%c

   iomsg = 'dtioread'

end subroutine
