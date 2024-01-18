!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 26, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : boz-literal args to REAL, INT, CMPLX and DBLE intrinsics
!*  SECONDARY FUNCTIONS TESTED : warning message displayed when -qport=typlssarg is used
!*                               with -qlanglvl=2003std
!*
!*  REQUIRED COMPILER OPTIONS  : -qlanglvl=2003std -qport=typlssarg
!*
!*  DESCRIPTION                : Diagnostic testing of the conflicting options
!*                               -qlanglvl=2003std and -qport=typlssarg. Genereates
!*                               a warning message.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
complex i


i=cmplx(z'144', z'144')

end program


