! *********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/run.sh fxbind_c03rrd  cxbind_c03rrd
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : fxbind_c03rrd.f
!* TEST CASE TITLE              : BIND(C) for Fortran procedures 
!*
!* PROGRAMMER                   : Kan Tian
!* DATE                         : Jan, 7, 2004
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     :Interoperable Functions.
!*                             
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf95
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: BINC(C) attribute with derived type array.
!*   - using external FORTRAN functions
!*   - passing 1-dim  derived type array arguments,
!*     represented as an array of structures in C.
!*   - main written in C     
!*
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/07/04   KT     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE Personal_details
  USE ISO_C_BINDING
  IMPLICIT NONE
  TYPE, BIND(C)::Person
     REAL:: Weight
     INTEGER :: Age
     CHARACTER :: Sex
  END TYPE Person
END MODULE Personal_details

Function Stats(Data,No,M_a,F_a) Bind(c)
  USE Personal_details
  IMPLICIT NONE
  REAL :: Stats
  TYPE(Person), DIMENSION(No)::Data
  REAL :: M_a,F_a
  INTEGER:: No
  INTEGER :: I,No_f,No_m
  M_a=0.0; F_a=0.0;No_f=0; No_m =0
  DO I=1,No
     IF ( Data(I)%Sex == 'M' &
          .OR. Data(I)%Sex == 'm') THEN
        M_a=M_a+Data(I)%Weight
        No_m=No_m+1
     ELSEIF(Data(I)%Sex == 'F' &
          .OR. Data(I)%Sex == 'f') THEN
        F_a=F_a +Data(I)%Weight
        No_f=No_f+1
     ENDIF
  END DO
  IF (No_m > 0 ) THEN
     M_a = M_a/No_m
  ENDIF
  IF (No_f > 0 ) THEN
     F_a = F_a/No_f
  ENDIF
  Stats = M_a
END Function  Stats
