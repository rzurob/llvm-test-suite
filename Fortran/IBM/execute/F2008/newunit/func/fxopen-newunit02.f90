! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : fxopen-newunit02.f
!*
!* PROGRAMMER                   : Sarah Kouchaki-Ramezan
!* DATE                         : Oct. 2010
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : F2002: NEWUNIT= specIFier, Feature#:377344
!* SECONDARY FUNTIONS TESTED    : READ,WRITE,REWIND
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Open many files with dIFferent NEWUNIT value
!*                                being ensure there isn't any dup
!*
!* read *,files(i)
!* write(files(i),'("f",i4)') i+256
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  10/10/21    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    PROGRAM FXOPEN_NEWUNIT02
      IMPLICIT NONE

      CHARACTER (LEN=15) :: files(256)
      INTEGER :: i=1,IVAR(256),n=256,j,SVAR

      DO WHILE (i .LE. n)
      WRITE(files(i),'("f",i4)') i+256
      OPEN(NEWUNIT=IVAR(i), FILE=files(i),ACTION='readwrite')
      SVAR=IVAR(i)

      IF ( SVAR >= -2 ) THEN
      ERROR STOP 2_4
      ENDIF

!**********************************************************
!        Checking the NEWUNIT=value                       *
!**********************************************************

      DO j=1, i-1
       IF (SVAR .eq. IVAR(j)) THEN
       PRINT *, SVAR, IVAR(j),j
       ERROR STOP 4_4
       ENDIF
      END DO

      i=i+1
      END DO

      DO i = 1, 256
      ClOSE(IVAR(i))
      END DO

      END PROGRAM FXOPEN_NEWUNIT02
