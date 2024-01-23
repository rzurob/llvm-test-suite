!*  ===================================================================
!*
!*  DATE                       : March 15, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() Statement on (Formatted) I/O Units
!*  SECONDARY FUNCTIONS TESTED : WAIT() on Invalid ID= Values that are
!*                               scalar-int-expr (both Constants and simple
!*                               Expressions)
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
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM idSpecIDValue09

    INTEGER, DIMENSION( 10 ) :: ioID
    INTEGER, DIMENSION( 10 ) :: dataValues

    CHARACTER(LEN = 256) :: iMsg


    OPEN(460, FILE='idSpecIDValue09.dat',&
        &FORM='formatted', ASYNCHRONOUS='yes',&
        &ACTION='read', ACCESS='stream', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN(idSpecIDValue09.dat,Read) <", iStat, "> ", iMsg
        ERROR STOP 1
    END IF

    OPEN(461, ASYNCHRONOUS='yes', ACTION='write',&
        &FORM='formatted', ACCESS='stream', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN(461,Write) <", iStat, "> ", iMsg
        ERROR STOP 2
    END IF


    DO i = 1, 10
        READ(460, '(I5)', ASYNCHRONOUS='yes',&
            &ID=ioID( i ), IOSTAT=iStat, IOMSG=iMsg) dataValues( i )

        IF (iStat <> 0) THEN
            WRITE(0, *) i, ") READ(idSpecIDValue09.dat) <", iStat, "> ", iMsg
            CALL zzrc( (10 + i) )
        END IF

        ioID( i ) = (ioID( i ) + 3) * 4
    END DO


    DO i = 10, 1, -1
        WAIT(460, ID=((ioID( i ) - 4) / 3), IOSTAT=iStat, IOMSG=iMsg)

        WRITE(0, *) i, ") WAIT(idSpecIDValue09.dat) <", iStat, "> ", iMsg

        IF (iStat <> 224) THEN
            CALL zzrc( (20 + i) )
        END IF
    END DO


    CLOSE(460, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE(idSpecIDValue09.dat) <", iStat, "> ", iMsg
        ERROR STOP 31
    END IF


    DO i = 1, 10
        WRITE(461, '(I5)', ASYNCHRONOUS='yes',&
            &ID=ioID( i ), IOSTAT=iStat, IOMSG=iMsg) dataValues( i )

        IF (iStat <> 0) THEN
            WRITE(0, *) i, ") WRITE(461) <", iStat, "> ", iMsg
            CALL zzrc( (40 + i) )
        END IF

        ioID( i ) = (ioID( i ) * 4) + (i * 3)
    END DO


    DO i = 10, 1, -1
        WAIT(461, ID=((ioID( i ) / 3) - (i * 4)), IOSTAT=iStat, IOMSG=iMsg)

        WRITE(0, *) i, ") WAIT(461) <", iStat, "> ", iMsg

        IF (iStat <> 224) THEN
            CALL zzrc( (50 + i) )
        END IF
    END DO


    CLOSE(461, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE(461) <", iStat, "> ", iMsg
        ERROR STOP 61
    END IF

END PROGRAM idSpecIDValue09
