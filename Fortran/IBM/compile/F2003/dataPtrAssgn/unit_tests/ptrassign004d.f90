!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign004.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 31, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                :C718
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  real , pointer :: ptr1(:,:,:)

  real, target :: arr1(1:5,2:6,3:7)

  !incorrect, too few bounds-spec
  ptr1(4:,5:)=>arr1

  !incorrect, too many bounds-spec
  ptr1(4:,5:,6:,7:) => arr1


end

