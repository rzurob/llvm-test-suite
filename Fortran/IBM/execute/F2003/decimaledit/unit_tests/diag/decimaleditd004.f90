!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: decimaleditd004.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 02, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : providing support for the DECIMAL=
!*                               specifier and decimal edit mode control
!*                               descriptors. Feature 289039.
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*  REQUIRED RUNTIME OPTIONS   : langlvl=xxxxx
!*
!*  DESCRIPTION                : This diagnostic test, checks various
!*                               runtime langlvls to make sure use of
!*                               DC and DP descriptors gets flagged
!*                               when 90std and 95std is specified.
!*                               This only tests runtime encoding of
!*                               the descriptors.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      CHARACTER(10) BUFFER
      real :: TMP
      CHARACTER(10) MY_FMT

      open(unit=77, file='decimaleditd004.dat')

! TESTING DC EDIT DESCRIPTOR:
      MY_FMT = '(DC,F4.2)'
      WRITE(*,MY_FMT) 3.14
      READ(77,MY_FMT) TMP

      ! test for internal files as well:
      WRITE(BUFFER, MY_FMT) 3.14
      READ(BUFFER, MY_FMT) TMP

! TESTING DP EDIT DESCRIPTOR:
      MY_FMT = '(DP,F4.2)'
      WRITE(*,MY_FMT) 3.14
      READ(77,MY_FMT) TMP

      ! test for internal files as well:
      WRITE(BUFFER, MY_FMT) 3.14
      READ(BUFFER, MY_FMT) TMP

      END
