!*  ===================================================================
!*
!*  DATE                       : March  7, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() on specific Pending (Unformatted)
!*                               Data Transfer Operations
!*  SECONDARY FUNCTIONS TESTED : WAIT() on *ALL* (remaining) Pending Data
!*                               Transfer Operations
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WAIT(), ID= Specifier
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
!*  The value of the expression specified in the ID= specifier shall be the
!*  identifier of a pending data transfer operation for the specified unit.
!*  If the ID= specifier appears, a wait operation for the specified data
!*  transfer operation is performed. If the ID= specifier is omitted, wait
!*  operations for all pending data transfers for the specified unit are
!*  performed.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM noIDSpecPending02

    INTEGER, DIMENSION( 10 ) :: ids

    CHARACTER(LEN = 256) :: iMsg


    OPEN(UNIT=77, ASYNCHRONOUS='yes', ACTION='write',&
        &ACCESS='stream', FORM='unformatted', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        ERROR STOP 1
    END IF


    DO i = 1, 10
        WRITE(77, ASYNCHRONOUS='yes', ID=ids( i ),&
                    &IOSTAT=iStat, IOMSG=iMsg) (i - 1)
        IF (iStat <> 0) THEN
            WRITE(0, *) "WRITE() <", iStat, "> ", iMsg
            CALL zzrc( (10 + i) )
        END IF
    END DO


    !
    !  Wait on Odd Numbered (1, 3, 5, 7, 9) Pending Data Transfer IDs.
    !  (Should be successful.)
    !
    CALL Wait4IDs(77, ids, 1, 0, 20)

    !
    !  Wait on *ALL* remaining Pending Data Transfer IDs.
    !
    WAIT(77, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
        ERROR STOP 30
    END IF

    !
    !  Wait on Even Numbered (2, 4, 6, 8, 10) Pending Data Transfer IDs.
    !  (Should experience a run-time failure.)
    !
    CALL Wait4IDs(77, ids, 2, 224, 40)


    CLOSE(77, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        ERROR STOP 50
    END IF

END PROGRAM noIDSpecPending02


SUBROUTINE Wait4IDs(ioUnit, ioIDs, start, ioStatExpected, failRCbase)

    INTEGER :: ioUnit
    INTEGER, DIMENSION( 10 ) :: ioIDs
    INTEGER :: start
    INTEGER :: ioStatExpected
    INTEGER :: failRCbase

    CHARACTER(LEN = 256) :: iMsg = ''


    DO i = start, 10, 2
        WAIT(UNIT=ioUnit, ID=ioIDs( i ), IOSTAT=iStat, IOMSG=iMsg)
        IF (iStat <> ioStatExpected) THEN
            WRITE(0, *) "WAIT(", ioIDs( i ), ") <", iStat, "> ", iMsg
            CALL zzrc( (failRCbase + i) )
        END IF
    END DO

END SUBROUTINE Wait4IDs
