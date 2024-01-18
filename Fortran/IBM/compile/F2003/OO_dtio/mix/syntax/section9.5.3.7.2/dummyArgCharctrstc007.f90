! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Characteristics of DTIO interface and procedures
!*                               shall be the same as ones defined in Section 9.5.3.7.2.
!*                               1) Dummy Argument Characteristics
!*                                  -  pointer, target, and allocatable
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
      character(3) :: c
   end type
end module

program dummyArgCharctrstc007
   use m

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(inout), allocatable :: dtv    !<- first argument defines to be allocatable
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in), pointer :: dtv           !<- first argument defines to be pointer
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface read(formatted)
      subroutine readFormatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base), intent(inout), target :: dtv         !<- first argument defines to be a target
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in) :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface write(formatted)
      subroutine writeFormatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer, pointer, intent(in) :: unit              !<- defines to be pointer
         character(*), intent(in), allocatable :: iotype   !<- defines to be allocatable
         integer, allocatable, intent(in) :: v_list(:)     !<- defines to be allocatable
         integer, intent(out), allocatable :: iostat       !<- defines to be allocatable
         character(*), intent(inout), target :: iomsg      !<- defines to be target
      end subroutine
   end interface

end program
