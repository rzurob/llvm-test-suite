!*  ===================================================================
!*
!*  DATE                       : March 15, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() Statement
!*  SECONDARY FUNCTIONS TESTED : WAIT() on ID= Value from the same and
!*                               other (Formatted) I/O Units
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WAIT(), ID= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  Due to the implementation of Formatted Asynchronous I/O, all of the
!*  WAIT() Statements below should complete with an IOSTAT value of 0.
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

PROGRAM idSpecIDValue07
    IMPLICIT NONE

    INTERFACE
        SUBROUTINE PerformIO(unitList, n, aIDs, dataList, failRCbase)
            IMPLICIT NONE

            INTEGER, DIMENSION( 10 ), INTENT(IN) :: unitList
            INTEGER, INTENT(IN) :: n
            INTEGER, DIMENSION( 10 ), INTENT(INOUT) :: aIDs
            INTEGER, DIMENSION( 10,2 ), INTENT(INOUT) :: dataList
            INTEGER, INTENT(IN) :: failRCbase
        END SUBROUTINE PerformIO
    END INTERFACE

    INTEGER :: i
    INTEGER :: j

    INTEGER :: iStat

    INTEGER, DIMENSION( 10 ) :: aIDs
    INTEGER, DIMENSION( 10,2 ) :: dataValues

    INTEGER, DIMENSION( 10 ) :: ioUnits = (/ (i, i = 11, 20) /)

    CHARACTER(LEN = 5) :: action
    CHARACTER(LEN = 256) :: iMsg


    DO i = 1, 10
        action = 'write'
        IF (MOD(i, 2) == 0) THEN
            action = 'read'
        END IF

        OPEN(ioUnits( i ), ACTION=action, ACCESS='direct', RECL=5,&
            &FORM='formatted', ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=iMsg)

        IF (iStat <> 0) THEN
            WRITE(0, *) "OPEN(", ioUnits( i ), ",", action,&
                                    &") <", iStat, "> ", iMsg
            CALL zzrc( i )
        END IF
    END DO


    dataValues = RESHAPE((/ (((i * 321), i = 1, 10), j = 1, 2) /), (/ 10,2 /))

    CALL PerformIO(ioUnits, 1, aIDs, dataValues, 10)

    DO i = 1, 10
        WAIT(ioUnits( i ), ID=aIDs( i ), IOSTAT=iStat, IOMSG=iMsg)

        IF (iStat <> 0) THEN
            WRITE(0, *) "WAIT(", ioUnits( i ), ") <", iStat, "> ", iMsg
            CALL zzrc( (20 + i) )
        END IF
    END DO


    CALL PerformIO(ioUnits, 2, aIDs, dataValues, 30)

    DO i = 10, 1, -1
        j = 11 - i

        WAIT(ioUnits( j ), ID=aIDs( i ), IOSTAT=iStat, IOMSG=iMsg)

        IF (iStat <> 0) THEN
            WRITE(0, *) "WAIT(", ioUnits( j ), ") <", iStat, "> ", iMsg
            CALL zzrc( (40 + j) )
        END IF
    END DO


    DO i = 1, 10
        CLOSE(ioUnits( i ), IOSTAT=iStat, IOMSG=iMsg)
        IF (iStat <> 0) THEN
            WRITE(0, *) "CLOSE(", ioUnits( i ), ") <", iStat, "> ", iMsg
            CALL zzrc( (50 + i) )
        END IF
    END DO


    DO i = 1, 10
        WRITE(6, '(I2,")",2I5)') i, (dataValues( i,j ), j = 1, 2)
    END DO

END PROGRAM idSpecIDValue07


SUBROUTINE PerformIO(unitList, n, aIDs, dataList, failRCbase)
    IMPLICIT NONE

    INTEGER, DIMENSION( 10 ), INTENT(IN) :: unitList
    INTEGER, INTENT(IN) :: n
    INTEGER, DIMENSION( 10 ), INTENT(INOUT) :: aIDs
    INTEGER, DIMENSION( 10,2 ), INTENT(INOUT) :: dataList
    INTEGER, INTENT(IN) :: failRCbase

    INTEGER :: i
    INTEGER :: j
    INTEGER :: iStat

    CHARACTER(LEN = 256) :: iMsg


    DO i = 1, 10
        IF (MOD(i, 2) == 0) THEN
            READ(unitList( i ), 100, REC=i, ASYNCHRONOUS='yes',&
                &ID=aIDs( i ), IOSTAT=iStat, IOMSG=iMsg) dataList( i,n )

            IF (iStat <> 0) THEN
                WRITE(0, *) "READ(", unitList( i ), ") <", iStat, "> ", iMsg
                CALL zzrc( (i + failRCbase) )
            END IF

        ELSE
            WRITE(unitList( i ), 100, REC=i, ASYNCHRONOUS='yes',&
                &ID=aIDs( i ), IOSTAT=iStat, IOMSG=iMsg) dataList( i,n )

            IF (iStat <> 0) THEN
                WRITE(0, *) "WRITE(", unitList( i ), ") <", iStat, "> ", iMsg
                CALL zzrc( (i + failRCbase) )
            END IF
        END IF
    END DO

100 FORMAT(I5)

END SUBROUTINE PerformIO
