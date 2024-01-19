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
!*                                subroutine with formatted write,
!*                                read,rewind using FILE in open
!*                                statement.
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  10/10/21    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    PROGRAM FXOPEN_NEWUNIT15

    INTERFACE
       SUBROUTINE SUB_IVAR(IVAR1)
        INTEGER IVAR1
       END SUBROUTINE SUB_IVAR
    END INTERFACE

    IMPLICIT NONE

    INTEGER :: IVAR,i

    CHARACTER(LEN = 10) acc
    CHARACTER(LEN = 3) asyn
    CHARACTER(LEN = 7) stat

    asyn = 'yes'
    acc = 'sequential'
    stat = 'replace'

    OPEN(NEWUNIT=IVAR, ACCESS=acc, FILE='fxopen-newunit15.dat', &
      &ACTION='write',ASYNCHRONOUS=asyn, STATUS=stat)

    DO i = 1, 50
    WRITE(IVAR, 20) i,IVAR
20  FORMAT(2(50I2))
    END DO

    CALL SUB_IVAR(IVAR)

    FLUSH (IVAR)
    BACKSPACE(IVAR)

    DO i = 1, 50
    WRITE (IVAR,30) i , IVAR
30  FORMAT(2( 50I2 ))
    END DO


    CLOSE(IVAR)

    END PROGRAM FXOPEN_NEWUNIT15

    SUBROUTINE SUB_IVAR(IVAR1)
    INTEGER, intent(inout) :: IVAR1
    INTEGER :: i

    FLUSH (IVAR1)
    BACKSPACE(IVAR1)

    DO i = 1, 50
    WRITE (IVAR1,30) i , IVAR1
30  FORMAT(2( 50I2 ))
    END DO

    RETURN

    END SUBROUTINE SUB_IVAR


