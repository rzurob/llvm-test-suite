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
!* DESCRIPTION                  : NEWUNIT with,BACKSPACE,READ,WRITE,REWIND
!*                                FILE,FLUSH,STATUS='replace'
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  10/10/21    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    PROGRAM FXOPEN_NEWUNIT09
    IMPLICIT NONE

    INTEGER i /0/,j /0/,IVAR
    INTEGER Iarray(5,5)

    OPEN (NEWUNIT=IVAR,  status='scratch')

    DO i = 1,5
     DO j = 1, 5
        Iarray(i,j)=i*10+j
     END DO
    END DO

    WRITE(IVAR, *) Iarray(1,1)
    print *,  Iarray(1,1)
    WRITE(IVAR, *) Iarray(1,2)
    print *,  Iarray(1,2)

    FLUSH (IVAR)
    BACKSPACE(IVAR)

    WRITE (IVAR, *) Iarray(2,2)
    print *,  Iarray(2,2)
    WRITE (IVAR, *) Iarray(3,3)
    print *,  Iarray(3,3)

    REWIND(IVAR)

    READ(IVAR, * ) i
    print *, i,Iarray(1,1)
    READ(IVAR, *) j
    print *,j,Iarray(2,2)

    CLOSE(IVAR)

    OPEN (NEWUNIT=IVAR,  FILE='fxopen-newunit09.dat', status='replace')

    WRITE(IVAR, *) Iarray(2,2)
    print *,  Iarray(2,2)
    WRITE(IVAR, *) Iarray(3,3)
    print *,  Iarray(3,3)

    FLUSH (IVAR)
    BACKSPACE(IVAR)

    WRITE (IVAR, *) Iarray(1,1)
    print *,  Iarray(1,1)
    WRITE (IVAR, *) Iarray(1,2)
    print *,  Iarray(1,2)

    REWIND(IVAR)

    READ(IVAR, * ) i
    print *, i,Iarray(2,2)
    READ(IVAR, *) j
    print *,j,Iarray(1,1)

    CLOSE(IVAR)
    END PROGRAM FXOPEN_NEWUNIT09
