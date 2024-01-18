!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: decimaleditd003.f
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
!*  REQUIRED COMPILER OPTIONS  : -qdebug=intmsg  -qlanglvl=xxxxx
!*  REQUIRED RUNTIME OPTIONS   :
!*
!*  DESCRIPTION                : This diagnostic test, checks various
!*                               langlvls to make sure use of DC and DP
!*                               descriptors gets flagged when 77/95/95 std
!*                               or 90/95 pure is used. This only tests
!*                               compile-tme encoding of the descriptors.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      CHARACTER(10) BUFFER
      CHARACTER(10) TMP

! TESTING DC EDIT DESCRIPTOR:

      WRITE(*,'(DC, F4.2)') 3.14
      READ(*,'(DC, F4.2)') TMP
      WRITE(*,100) 3.14
      READ(*,100) TMP

      ! test for internal files as well:
      WRITE(BUFFER,'(DC, F4.2)') 3.14
      READ(BUFFER, '(DC, F4.2)') TMP
      WRITE(BUFFER, 100) 3.14
      READ(BUFFER, 100) TMP

 100  FORMAT(DC, F4.2)

! TESTING DP EDIT DESCRIPTOR:

      WRITE(*,'(DP, F4.2)') 3.14
      READ(*,'(DP, F4.2)') TMP
      WRITE(*,200) 3.14
      READ(*,200) TMP

      ! test for internal files as well:
      WRITE(BUFFER,'(DP, F4.2)') 3.14
      READ(BUFFER, '(DP, F4.2)') TMP
      WRITE(BUFFER, 200) 3.14
      READ(BUFFER, 200) TMP

 200  FORMAT(DP, F4.2)

      END
