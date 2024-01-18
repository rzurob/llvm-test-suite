!*  ===================================================================
!*
!*  DATE                       : March  7, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() on *ALL* Pending (Unformatted) Data
!*                               Transfer Operations for a Specific Unit
!*  SECONDARY FUNCTIONS TESTED : Pending Data Transfer Operations for other
!*                               Units should be unaffected
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

PROGRAM noIDSpecPending03

    INTEGER, DIMENSION( 10 ) :: ioUnit = (/ (i, i = 11, 20) /)

    INTEGER, DIMENSION( 10,100 ) :: ids
    INTEGER, DIMENSION( 10,100 ) :: dataValues

    CHARACTER(LEN = 256) :: iMsg


    DO i = 1, 10
        IF (MOD(i, 2) == 0) THEN
            OPEN(ioUnit( i ), ASYNCHRONOUS='yes',&
                &ACTION='read', FORM='unformatted',&
                &ACCESS='stream', IOSTAT=iStat, IOMSG=iMsg)
            IF (iStat <> 0) THEN
                WRITE(0, *) "OPEN(", ioUnit( i ),&
                            &",Read) <", iStat, "> ", iMsg
                CALL zzrc( i )
            END IF

        ELSE
            OPEN(ioUnit( i ), ASYNCHRONOUS='yes',&
                &ACTION='write', FORM='unformatted',&
                &ACCESS='stream', IOSTAT=iStat, IOMSG=iMsg)
            IF (iStat <> 0) THEN
                WRITE(0, *) "OPEN(", ioUnit( i ),&
                            &",Write) <", iStat, "> ", iMsg
                CALL zzrc( i )
            END IF
        END IF
    END DO


    dataValues = RESHAPE((/ ((i, j = 1, 10), i = 1, 100) /), (/ 10, 100 /))


    DO i = 1, 100
        DO j = 1, 10
            IF (MOD(j, 2) == 0) THEN
                READ(ASYNCHRONOUS='yes', UNIT=ioUnit( j ),&
                    &ID=ids( j,i ), IOSTAT=iStat, IOMSG=iMsg) dataValues( j,i )
                IF (iStat <> 0) THEN
                    WRITE(0, *) "READ(", ioUnit( j ), ") <", iStat, "> ", iMsg
                    CALL zzrc( (10 + j) )
                END IF

            ELSE
                WRITE(ASYNCHRONOUS='yes', UNIT=ioUnit( j ),&
                    &ID=ids( j,i ), IOSTAT=iStat, IOMSG=iMsg) dataValues( j,i )
                IF (iStat <> 0) THEN
                    WRITE(0, *) "WRITE(", ioUnit( j ), ") <", iStat, "> ", iMsg
                    CALL zzrc( (10 + j) )
                END IF
            END IF
        END DO
    END DO


    DO j = 10, 1, -1
        IF (MOD(j, 3) == 0) THEN
            WAIT(ioUnit( j ), IOSTAT=iStat, IOMSG=iMsg)
            IF (iStat <> 0) THEN
                WRITE(0, *) "WAIT(", ioUnit( j ), ") <", iStat, "> ", iMsg
                CALL zzrc( (20 + j) )
            END IF
        END IF
    END DO


    DO i = 1, 100
        DO j = 1, 10
            WAIT(ioUnit( j ), ID=ids( j,i ), IOSTAT=iStat, IOMSG=iMsg)

            IF (MOD(j, 3) == 0) THEN
                IF (iStat <> 224) THEN
                    WRITE(0, *) "WAIT(", ioUnit( j ), ") <", iStat, "> ", iMsg
                    CALL zzrc( (30 + j) )
                END IF

            ELSE
                IF (iStat <> 0) THEN
                    WRITE(0, *) "WAIT(", ioUnit( j ), ") <", iStat, "> ", iMsg
                    CALL zzrc( (30 + j) )
                END IF
            END IF
        END DO

        WRITE(6, '(5I4)') (dataValues( j,i ), j = 1, 10, 2)
    END DO


    DO j = 10, 1, -1
        CLOSE(ioUnit( j ), IOSTAT=iStat, IOMSG=iMsg)
        IF (iStat <> 0) THEN
            WRITE(0, *) "CLOSE(", ioUnit( j ), ") <", iStat, "> ", iMsg
            CALL zzrc( (40 + j) )
        END IF
    END DO

END PROGRAM noIDSpecPending03
