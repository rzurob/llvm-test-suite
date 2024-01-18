!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: bozlitargsIntrnFunc003.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 26, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : boz-literal args to REAL, INT, CMPLX and DBLE intrinsics
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qlanglvl=2003std
!*
!*  DESCRIPTION                : Generating Language level message when a boz-literal
!*                               is passed to REAL and INT when -qxlf2003=bozlitargs
!*                               is off and -qlanglvl=2003std is on.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
    integer :: i
    real :: j

    i=int(b'1100100')
    j=real(o'144')


end program


