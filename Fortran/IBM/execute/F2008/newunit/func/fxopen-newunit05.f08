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
!* DESCRIPTION                  : Passing NEWUNIT value to external
!*                                subroutine
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  10/10/21    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    PROGRAM FXOPEN_NEWUNIT05
    IMPLICIT NONE

    INTERFACE
       SUBROUTINE SUB_IVAR(IVAR1)
        INTEGER, intent(inout) :: IVAR1
       END SUBROUTINE SUB_IVAR
    END INTERFACE


    INTEGER :: IVAR,i,k(50)

    CHARACTER(LEN = 10) acc
    CHARACTER(LEN = 3) asyn
    CHARACTER(LEN = 7) stat
!**********************************************************
!        Initialization of variables                      *
!**********************************************************

    asyn = 'yes'
    acc = 'sequential'
    stat = 'scratch'

!**********************************************************
!        Writing and Reading the file                     *
!**********************************************************

    OPEN(NEWUNIT=IVAR, ACCESS=acc, ASYNCHRONOUS=asyn, STATUS=stat, ACTION='readwrite')

    DO i = 1, 50
    WRITE(IVAR, 20, ASYNCHRONOUS='yes') i
20  FORMAT(50I2)
    END DO

!**********************************************************
!       Passing NEWUNIT=value external SUB                *
!**********************************************************

    CALL SUB_IVAR(IVAR)

     REWIND(IVAR)

!**********************************************************
!        Checking the Results                             *
!**********************************************************

     DO i = 1, 50
     READ (IVAR,*) i
     k(i)=i
     END DO

    DO i = 1, 50
    WRITE (*,30) i , k(i), IVAR
30  FORMAT(3( 50I2 ))
    END DO

    CLOSE(IVAR)

    END PROGRAM FXOPEN_NEWUNIT05

    SUBROUTINE SUB_IVAR(IVAR1)
    INTEGER, intent(inout) :: IVAR1
    INTEGER :: k(50),i

    REWIND(IVAR1)

    DO i = 1, 50
    READ (IVAR1,*) i
    k(i)=i
    END DO

    DO i = 1, 50
    WRITE (*,30) i , k(i), IVAR1
30  FORMAT(3( 50I2 ))
    END DO

    RETURN
    END SUBROUTINE SUB_IVAR


