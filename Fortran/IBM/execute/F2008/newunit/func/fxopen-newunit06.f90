! *********************************************************************
!* ===================================================================
!*
!* DATE                         : Oct. 2010
!*
!* PRIMARY FUNCTIONS TESTED     : F2008: NEWUNIT= specifier, Feature#:377344
!* SECONDARY FUNTIONS TESTED    : READ,WRITE,REWIND
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Open several NEWUNIT value and
!*                                closing one NEWUNIT value and being ensure there
!*                                isn't any dup with opening new one
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  10/10/21    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    PROGRAM FXOPEN_NEWUNIT06
    IMPLICIT NONE

    INTEGER , PARAMETER :: NUM = 50
    INTEGER :: IVAR(NUM),RVAR(50),i,j=15

    CHARACTER(LEN = 10) acc
    CHARACTER(LEN = 3) asyn
    CHARACTER(LEN = 7) stat
!**********************************************************
!        Initialization of variables                      *
!**********************************************************

    asyn = 'yes'
    acc = 'sequential'
    stat = 'scratch'

    DO i = 1, NUM
    OPEN(NEWUNIT=IVAR(i), ACTION='write',ACCESS=acc, ASYNCHRONOUS=asyn, STATUS=stat)
    RVAR(i)=IVAR(i)

      IF ( IVAR(i) >= -2 ) THEN
      ERROR STOP 2_4
      ENDIF

    END DO

    ClOSE(IVAR(15))
!**********************************************************
!        Checking the NEWUNIT=value                       *
!**********************************************************

    OPEN(NEWUNIT=IVAR(j), ACCESS=acc, ASYNCHRONOUS=asyn, STATUS=stat)

      DO i=1, j-1
      IF ( ANY( RVAR .eq. IVAR(j)) )THEN
       PRINT *, RVAR, IVAR(j),j
       ERROR STOP 4_4
       ENDIF
      END DO

      DO i=j+1, NUM
       IF (ANY( RVAR .eq. IVAR(j)) )THEN
       PRINT *, RVAR, IVAR(j),j
       ERROR STOP 5_4
       ENDIF
      END DO

    DO i = 1, NUM
    ClOSE(IVAR(i))
    END DO

    END PROGRAM FXOPEN_NEWUNIT06
