!*  ===================================================================
!*
!*  DATE                       : March  8, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() on Pending Advancing Read Data
!*                               Transfer Operations (with/without ID=
!*                               Specifier) for a Specific Unit
!*  SECONDARY FUNCTIONS TESTED : EOR= Specifier
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WAIT(), EOR= Specifier, ID= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  9.6.1 WAIT statement
!*
!*  A WAIT statement performs a wait operation for specified pending
!*  asynchronous data transfer operations.
!*
!*  R921 wait-stmt  is  WAIT (wait-spec-list)
!*  R922 wait-spec  is  [ UNIT = ] file-unit-number
!*                  or  END = label
!*                  or  EOR = label
!*                  or  ERR = label
!*                  or  ID = scalar-int-expr
!*                  or  IOMSG = iomsg-variable
!*                  or  IOSTAT = scalar-int-variable
!*
!*  ...
!*
!*  NOTE 9.52
!*  An EOR= specifier has no effect if the pending data transfer operation
!*  is not a nonadvancing read.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM EORSpecRead01
    IMPLICIT NONE

    INTERFACE
        SUBROUTINE Wait4IDs(ioUnit, iIDs)
            IMPLICIT NONE

            INTEGER :: ioUnit
            INTEGER, DIMENSION( 100 ) :: iIDs
        END SUBROUTINE Wait4IDs
    END INTERFACE

    INTEGER :: i
    INTEGER :: j

    INTEGER, DIMENSION( 100 ) :: ioIDs
    INTEGER, DIMENSION( 10,100 ) :: dataValues

    INTEGER :: iStat

    CHARACTER(LEN = 256) :: iMsg


    OPEN(109, ASYNCHRONOUS='yes', ACCESS='sequential',&
        &ACTION='read', FORM='formatted', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        ERROR STOP 1
    END IF


    DO i = 1, 100
        READ(109, '(10I5)', IOMSG=iMsg, ASYNCHRONOUS='yes', ADVANCE='yes',&
                &IOSTAT=iStat, ID=ioIDs( i )) (dataValues( j,i ), j = 1, 10)

        IF (iStat <> 0) THEN
            WRITE(0, *) i, "READ() <", iStat, "> ", iMsg
            ERROR STOP 2
        END IF
    END DO


    CALL Wait4IDs(109, ioIDs)


    WAIT(IOSTAT=iStat, UNIT=109, IOMSG=iMsg, EOR=99)
    IF (iStat <> 0) THEN
        WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
        ERROR STOP 5
    END IF

    GOTO 100


99  CONTINUE
    WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
    ERROR STOP 6


100 CONTINUE
    DO i = 1, 100
        WRITE(6, '(I3,")",10I5)') i, (dataValues( j,i ), j = 1, 10)
    END DO


    CLOSE(109, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        ERROR STOP 7
    END IF

END PROGRAM EORSpecRead01


SUBROUTINE Wait4IDs(ioUnit, iIDs)
    IMPLICIT NONE

    INTEGER :: ioUnit
    INTEGER, DIMENSION( 100 ) :: iIDs

    INTEGER :: i
    INTEGER :: iStat

    CHARACTER(LEN = 256) :: iMsg

    DO i = 60, 40, -1
        WAIT(EOR=99, UNIT=ioUnit, ID=iIDs( i ), IOSTAT=iStat, IOMSG=iMsg)
        IF (iStat <> 0) THEN
            WRITE(0, *) i, ") WAIT(", iIDs( i ), ") <", iStat, "> ", iMsg
            ERROR STOP 3
        END IF
    END DO

    GOTO 100


99  CONTINUE
    WRITE(0, *) i, ") WAIT(", iIDs( i ), ") <", iStat, "> ", iMsg
    ERROR STOP 4


100 CONTINUE
    RETURN

END SUBROUTINE Wait4IDs
