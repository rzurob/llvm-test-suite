!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp dummyArgCharctrstc004.f
! %END
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
!*                                  -  different shape
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1

   type base1
      integer(4) :: i
   end type

end module

program dummyArgCharctrstc004
   use m1

   interface write(formatted)
      ! write : defines array of multiple rank instead of rank one
      subroutine write (dtv, unit, iotype, v_list, iostat, iomsg)
         import base1
         class(base1), intent(in) :: dtv(1)                  !<- shall be a scalar instead of rank-one array
         integer, intent(in), dimension(1) :: unit           !<- shall be a scalar instead of rank-one array
         character(*), intent(in) :: iotype
         integer, intent(in) :: v_list(:,:)                  !<- shall be assumed shape array of rank one
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

    interface read(formatted)
       ! read : defines array of multiple rank instead of rank one
       subroutine read (dtv, unit, iotype, v_list, iostat, iomsg)
          import base1
          class(base1), intent(inout) :: dtv
          integer, intent(in) :: unit
          character(*), intent(in), dimension(:) :: iotype   !<- shall be scalar
          integer, intent(in) :: v_list(*)                   !<- shall be assumed shape array of rank one
          integer, intent(out) :: iostat
          character(*), intent(inout), dimension(1) :: iomsg !<- shall be scalar instead of rank-one array
       end subroutine
   end interface

end program





