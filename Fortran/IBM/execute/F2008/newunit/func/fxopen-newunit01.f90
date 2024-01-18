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
!* DESCRIPTION                  : Simple test for NEWUNIT in open statement
!*                                with STATUS=SCRATCH , ASYNCHRONOUS=yes,ACTION=readwrite,
!*                                FORM=Formatted. Use formatted write,read and using
!*                                rewind.
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  10/10/20    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    PROGRAM FXOPEN_NEWUNIT01
    IMPLICIT NONE

    INTEGER :: IVAR,i,j(50),k(50)
    CHARACTER(LEN = 10) acc
    CHARACTER(LEN = 3) asyn
    CHARACTER(LEN = 7) stat

!**********************************************************
!        Initialization of variables                      *
!**********************************************************

    asyn = 'yes'
    acc = 'sequential'
    stat = 'scratch'

    DO i = 1, 50
    j(i) = i - 1
    END DO

!**********************************************************
!        Writing and Reading the file                     *
!**********************************************************

    OPEN(NEWUNIT=IVAR, ACTION='readwrite' ,ACCESS=acc, ASYNCHRONOUS=asyn, STATUS=stat, FORM='formatted')

    DO i = 1, 50
    WRITE(IVAR, 20) j(i)
20  FORMAT(50I2)
    END DO

    REWIND(IVAR)

    DO i = 1, 50
    READ (IVAR,*) j(i)
    k(i)=j(i)
    END DO

    DO i = 1, 50
    WRITE (*,30) i , k(i)
30  FORMAT(2( 50I2 ))
    END DO

    CLOSE(IVAR)

    END PROGRAM FXOPEN_NEWUNIT01
