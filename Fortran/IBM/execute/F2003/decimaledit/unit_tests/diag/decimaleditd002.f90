!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: decimaleditd002.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : decimaleditd002
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Dec. 02, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : providing support for the DECIMAL=
!*                               specifier and decimal edit mode control
!*                               descriptors. Feature 289039.
!*                                                   
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  : -qdebug=intmsg -qlanglvl=xxxxx
!*  REQUIRED RUNTIME OPTIONS   : 
!*
!*  DESCRIPTION                : This diagnostic test, checks various    
!*                               langlvls to make sure use of decimal=
!*                               specifier gets flagged when 77/95/95 std
!*                               or 90/95 pure is used.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      

      CHARACTER(5) MODE
      CHARACTER(10) BUFFER
      CHARACTER(10) TMP
      

      OPEN(UNIT=1,FILE='decimaleditd002.dat',DECIMAL='COMMA')
      WRITE(*,*, DECIMAL='COMMA') 3.14
      READ(1, *, DECIMAL='POINT') TMP 
      INQUIRE(UNIT=1, DECIMAL=MODE)

      ! test for internal files as well:
      WRITE(BUFFER,'(f4.2)', DECIMAL='COMMA') 3.14
      READ(BUFFER, '(f4.2)', DECIMAL='POINT') TMP
      
      END
