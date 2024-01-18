! *********************************************************************
!* ===================================================================
!*
!* DATE                         : Oct. 2010
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     : F2008: NEWUNIT= specifier, Feature#:377344
!* SECONDARY FUNTIONS TESTED    : READ,WRITE,REWIND
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Simple testcase for defect#381739
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  10/10/21    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    PROGRAM FXOPEN_NEWUNIT20
    IMPLICIT NONE

    INTEGER :: IVAR,i
    REAL RVAR


    OPEN(NEWUNIT=IVAR, FILE = 'fxopen-newunit20.dat')

    PRINT *, IVAR

    READ (IVAR, *) RVAR

    PRINT *,RVAR

    CLOSE(IVAR)

    END PROGRAM FXOPEN_NEWUNIT20
