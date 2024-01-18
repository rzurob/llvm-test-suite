!*  ===================================================================
!*
!*  DATE                       : March 14, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() with ID= Specifier on Pending
!*                               (Formatted) Data Transfer Operations
!*  SECONDARY FUNCTIONS TESTED : WAIT() on *ALL* Remaining Pending Data
!*                               Transfer Operations for the Specified Unit
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WAIT(), ID= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  Since Asynchronous Formatted I/O has been implemented differently from
!*  Asynchronous Unformatted I/O, all of the WAIT() Statments below should
!*  be Successful.
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

PROGRAM noIDSpecPending04

    INTEGER, DIMENSION( 100 ) :: ioID
    INTEGER, DIMENSION( 100 ) :: dataOut
    INTEGER, DIMENSION( 2,100 ) :: dataIn

    CHARACTER(LEN = 256) :: oMsg


    ioUnit = 44
    OPEN(ioUnit, ASYNCHRONOUS='yes', FORM='formatted',&
        &ACTION='readwrite', ACCESS='stream', IOSTAT=iStat, IOMSG=oMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", oMsg
        CALL zzrc( 1 )
    END IF


    dataOut = (/ (((i * 3) / 2 + 7), i = 1, 100) /)


    DO i = 1, 100
        WRITE(ioUnit, FMT='(2I5)', ASYNCHRONOUS='yes',&
            &ID=ioID( i ), IOSTAT=iStat, IOMSG=oMsg) i, dataOut( i )
        IF (iStat <> 0) THEN
            WRITE(0, *) i, ") WRITE() <", iStat, "> ", oMsg
            CALL zzrc( 2 )
        END IF
    END DO


    CALL WaitOnIDs(ioUnit, 2, ioID, 3)

    WAIT(ioUnit, IOSTAT=iStat, IOMSG=oMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "WAIT() <", iStat, "> ", oMsg
        CALL zzrc( 4 )
    END IF

    CALL WaitOnIDs(ioUnit, 1, ioID, 5)


    REWIND ioUnit


    DO i = 1, 100
        READ(ioUnit, FMT='(2I5)', ASYNCHRONOUS='yes',&
             &ID=ioID( i ), IOSTAT=iStat, IOMSG=oMsg)&
                                &(dataIn( j,i ), j = 1, 2)
        IF (iStat <> 0) THEN
            WRITE(0, *) i, ") READ() <", iStat, "> ", oMsg
            CALL zzrc( 6 )
        END IF
    END DO


    CALL WaitOnIDs(ioUnit, 2, ioID, 7)

    WAIT(ioUnit, IOSTAT=iStat, IOMSG=oMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "WAIT() <", iStat, "> ", oMsg
        CALL zzrc( 8 )
    END IF

    CALL WaitOnIDs(ioUnit, 1, ioID, 9)


    CLOSE(ioUnit, IOSTAT=iStat, IOMSG=oMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", oMsg
        CALL zzrc( 10 )
    END IF


    DO i = 1, 100
        IF (dataIn( 1,i ) <> i) THEN
            WRITE(6, *) "dataIn(1,", i, ") = '", dataIn( 1,i ), "'"
            WRITE(6, *) "Should be '", i, "'"
            CALL zzrc( 11 )

        ELSE IF (dataIn( 2,i ) <> dataOut( i )) THEN
            WRITE(6, *) "dataIn(2,", i, ") = '", dataIn( 2,i ), "'"
            WRITE(6, *) "dataOut(", i, ") = '", dataOut( i ), "'"
            CALL zzrc( 12 )
        END IF
    END DO

END PROGRAM noIDSpecPending04


SUBROUTINE WaitOnIDs(theUnit, increment, idList, failureCode)

    INTEGER, INTENT(IN) :: theUnit
    INTEGER, INTENT(IN) :: increment
    INTEGER, DIMENSION( 100 ), INTENT(IN) :: idList
    INTEGER, INTENT(IN) :: failureCode

    INTEGER :: wStat
    CHARACTER(LEN = 256) :: wMsg

    DO j = 1, 100, increment
        WAIT(theUnit, ID=idList( j ), IOSTAT=wStat, IOMSG=wMsg)
        IF (wStat <> 0) THEN
            WRITE(0, *) j, ") WAIT() <", wStat, "> ", wMsg
            CALL zzrc( failureCode )
        END IF
    END DO

END SUBROUTINE WaitOnIDs
