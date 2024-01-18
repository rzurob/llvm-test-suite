!*********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : fxopen-newunit11.f
!*
!* PROGRAMMER                   : Sarah Kouchaki-Ramezan
!* DATE                         : Oct. 2010
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : F2008: NEWUNIT= specifier, Feature#:377344
!* SECONDARY FUNTIONS TESTED    : READ,WRITE,REWIND
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Simple testcase for NEWUNIT wtih 
!*                                FILE, Write formated I/O
!*                                
!*                                
!*                                
!*                                
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  10/10/21    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    PROGRAM FXOPEN_NEWUNIT11
    IMPLICIT NONE

    INTEGER :: IVAR,i,j(50)

    CHARACTER(LEN = 10) acc
    CHARACTER(LEN = 3) asyn
    CHARACTER(LEN = 7) stat

    asyn = 'yes'
    acc = 'sequential'
    stat = 'scratch'

    DO i = 1, 50
    j(i) = i - 1
    END DO

    OPEN(NEWUNIT=IVAR, ACTION='write' ,ACCESS=acc, ASYNCHRONOUS=asyn,FILE='fxopen-newunit11.dat')

    DO i = 1, 50
    WRITE(IVAR, 20) j(i)
20  FORMAT(50I2)
    END DO

    CLOSE(IVAR)

    END PROGRAM FXOPEN_NEWUNIT11
