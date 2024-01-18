! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10.1.1 Namelist group object names
!*                                        Within the input data, each name shall correspond
!*                                        to a particular namelist group object name
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
      character(3) :: i
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
end module

program groupobj001d
   use m

   integer :: stat
   character(200) :: msg
   class(base), allocatable :: b1
   class(base), pointer     :: b2

   namelist /nml/ b1, b2
   allocate (b1, b2)

   open (1, file='groupobj001d.1', form='formatted', access='sequential' )
   open (2, file='groupobj001d.2', form='formatted', access='sequential' )

   read (1, nml, iostat = stat, iomsg = msg)
   print *, stat, msg

   if ( ( b1%i /= 'abc' ) .or. ( b2%i /= 'def' ) )  error stop 1_4

   read (2, nml, iostat = stat, iomsg = msg)
   print *, stat, msg

   if ( ( b1%i /= 'abc' ) .or. ( b2%i /= 'def' ) )  error stop 1_4

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= 'NAMELIST' ) error stop 2_4
   if ( size(v_list,1) /= 0 )  error stop 3_4

   read (unit, "(A3)", iostat=iostat )  dtv%i

   iomsg = 'dtioread'

end subroutine
