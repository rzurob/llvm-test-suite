! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : fxopen-newunit21.f
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
!* DESCRIPTION                  : Simple test for NEWUNIT in open statement 
!*                                with STATUS=SCRATCH , ASYNCHRONOUS=yes,ACTION=write, 
!*                                FORM=Formatted. Use formatted write,read and using 
!*                                rewind. Testing with -qintsize=1,2,4,8
!*                                
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  10/10/20    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    PROGRAM FXOPEN_NEWUNIT21
    IMPLICIT NONE

    INTEGER :: IVAR,i,j(50),k(50)

    CHARACTER(LEN = 10) acc
    CHARACTER(LEN = 3) asyn
    CHARACTER(LEN = 7) stat

    asyn = 'yes'
    acc = 'sequential'
    stat = 'scratch'

    DO i = 1, 50
    j(i) = i - 1
    END DO

    OPEN(NEWUNIT=IVAR, ACTION='readwrite' ,ACCESS=acc, ASYNCHRONOUS=asyn, STATUS=stat, FORM='formatted')

    if ( IVAR > -2 ) then
    print *, IVAR 
    error stop 2_4
    end if

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

    END PROGRAM FXOPEN_NEWUNIT21
