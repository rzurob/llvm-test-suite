!*  ===================================================================
!*
!*  DATE                       : March  8, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() on Pending Write Data Transfer
!*                               Operations (with/without ID= Specifier)
!*                               for a Specific Unit
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

PROGRAM EORSpecWrite01

    INTERFACE
        SUBROUTINE Wait4IDs(ioUnit, ioIDs)
            INTEGER, INTENT(IN) :: ioUnit
            INTEGER, DIMENSION( 1000 ), INTENT(IN) :: ioIDs
        END SUBROUTINE Wait4IDs
    END INTERFACE

    INTEGER :: oStat
    INTEGER, DIMENSION( 1000 ) :: oIDs

    CHARACTER(LEN = 256) :: oMsg

    OPEN(81, ACTION='write', FORM='formatted', IOMSG=oMsg,&
            &ASYNCHRONOUS='yes', ACCESS='stream', IOSTAT=oStat)
    IF (oStat <> 0) THEN
        WRITE(0, *) "OPEN() <", oStat, "> ", oMsg
        ERROR STOP 1
    END IF


    DO i = 1, 1000
        WRITE(81, '(5I5)', ASYNCHRONOUS='yes', ID=oIDs( i ),&
            &IOSTAT=oStat, IOMSG=oMsg) ((i * 10 + j * 10 + i + j), j = 1, 10)

        IF (oStat <> 0) THEN
            WRITE(0, *) "WRITE() <", oStat, "> ", oMsg
            ERROR STOP 2
        END IF
    END DO


    CALL Wait4IDs(81, oIDs)


    WAIT(81, EOR=100, IOSTAT=oStat, IOMSG=oMsg)
    IF (oStat <> 0) THEN
        WRITE(0, *) "WAIT() <", oStat, "> ", oMsg
        ERROR STOP 5
    END IF


    CLOSE(81, IOSTAT=oStat, IOMSG=oMsg)
    IF (oStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", oStat, "> ", oMsg
        ERROR STOP 7
    END IF

    STOP 0


100 CONTINUE
    WRITE(0, *) "EOR WAIT() <", oStat, "> ", oMsg
    ERROR STOP 6

END PROGRAM EORSpecWrite01


SUBROUTINE Wait4IDs(ioUnit, ioIDs)

    INTEGER, INTENT(IN) :: ioUnit
    INTEGER, DIMENSION( 1000 ), INTENT(IN) :: ioIDs

    INTEGER :: oStat
    CHARACTER(LEN = 256) :: oMsg


    DO i = 750, 250, -5
        WAIT(ioUnit, ID=ioIDs( i ), EOR=100, IOSTAT=oStat, IOMSG=oMsg)

        IF (oStat <> 0) THEN
            WRITE(0, *) i, ") WAIT(", ioIDs( i ), ") <", oStat, "> ", oMsg
            ERROR STOP 3
        END IF
    END DO

    RETURN


100 WRITE(0, *) i, ") EOR WAIT(", ioIDs( i ), ") <", oStat, "> ", oMsg
    ERROR STOP 4

END SUBROUTINE Wait4IDs
