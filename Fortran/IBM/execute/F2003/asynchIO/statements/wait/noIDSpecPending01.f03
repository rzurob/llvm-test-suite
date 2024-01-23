!*  ===================================================================
!*
!*  DATE                       : March  7, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() Statement (Unformatted I/O)
!*  SECONDARY FUNCTIONS TESTED : No ID= Specifier performs Wait Operations
!*                               for all Pending Data Transfers for the
!*                               specified Unit
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
!*  ... If the ID= specifier is omitted, wait operations for all pending data
!*  transfers for the specified unit are performed.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM noIDSpecPending01

    INTEGER, DIMENSION( 10 ) :: iIDs
    INTEGER, DIMENSION( 10 ) :: newValues
    INTEGER, DIMENSION( 10 ) :: dataValues = (/ (i, i = 1, 10) /)

    CHARACTER(LEN = 256) :: iMsg


    OPEN(111, ASYNCHRONOUS='yes', ACTION='readwrite', RECL=4,&
        &ACCESS='direct', FORM='unformatted', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        ERROR STOP 1
    END IF


    DO i = 1, 10
        WRITE(111, ASYNCHRONOUS='yes', ID=iIDs( i ),&
            &REC=i, IOSTAT=iStat, IOMSG=iMsg) dataValues( i )
        IF (iStat <> 0) THEN
            WRITE(0, *) "WRITE() <", iStat, "> ", iMsg
            CALL zzrc( (10 + i) )
        END IF
    END DO


    WAIT(111, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
        ERROR STOP 21
    END IF


    CALL Wait4IDs(111, iIDs, 30)


    DO i = 10, 1, -1
        READ(111, ASYNCHRONOUS='yes', ID=iIDs( i ),&
            &REC=i, IOSTAT=iStat, IOMSG=iMsg) newValues( i )
        IF (iStat <> 0) THEN
            WRITE(0, *) "WRITE() <", iStat, "> ", iMsg
            CALL zzrc( (40 + i) )
        END IF
    END DO


    WAIT(111, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
        ERROR STOP 51
    END IF


    CALL Wait4IDs(111, iIDs, 60)


    DO i = 1, 10
        IF (dataValues( i ) <> newValues( i )) THEN
            WRITE(0, *) i, ")  dataValues =", dataValues( i ),&
                                    ", newValues =", newValues( i )
            CALL zzrc( (70 + i) )
        END IF
    END DO


    CLOSE(111, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        ERROR STOP 80
    END IF

END PROGRAM noIDSpecPending01


SUBROUTINE Wait4IDs(ioUnit, ioIDs, failRCbase)
    INTEGER :: ioUnit
    INTEGER, DIMENSION( 10 ) :: ioIDs
    INTEGER :: failRCbase

    CHARACTER(LEN = 256) :: iMsg = ''


    DO i = 1, 10
        WAIT(ioUnit, ID=ioIDs( i ), IOSTAT=iStat, IOMSG=iMsg)
        IF (iStat <> 224) THEN
            WRITE(0, *) "WAIT(", ioIDs( i ), ") <", iStat, "> ", iMsg
            CALL zzrc( (i + failRCbase) )
        END IF
    END DO

END SUBROUTINE Wait4IDs
