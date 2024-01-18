!*  ===================================================================
!*
!*  DATE                       : March  7, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() Statement
!*  SECONDARY FUNCTIONS TESTED : ID= Specifier where the Value is a
!*                               scalar-int-expr that resolves to an
!*                               Identifier which is not associated
!*                               with the specified Unit
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

PROGRAM idSpecIDValue04d

    INTERFACE
        INTEGER FUNCTION MangleID( ioID )
            INTEGER, INTENT(IN) :: ioID
        END FUNCTION MangleID

        SUBROUTINE OpenAndIO(i, ioUnit, act, dataValue, ioID)
            INTEGER, INTENT(IN) :: i
            INTEGER, INTENT(IN) :: ioUnit
            CHARACTER(LEN = 5), INTENT(IN) :: act
            INTEGER, ASYNCHRONOUS, INTENT(INOUT) :: dataValue
            INTEGER, ASYNCHRONOUS, INTENT(INOUT) :: ioID
        END SUBROUTINE OpenAndIO
    END INTERFACE


    INTEGER, DIMENSION( 10 ) :: ids
    INTEGER, DIMENSION( 10 ) :: newIDs

    INTEGER, DIMENSION( 10 ) :: unitNum = (/ ((i + 10), i = 1, 10) /)
    INTEGER, DIMENSION( 10 ) :: dataValue = (/ (33, i = 1, 10) /)

    CHARACTER(LEN = 5) :: act
    CHARACTER(LEN = 256) :: iMsg


    DO i = 1, 10
        IF (MOD(i, 2) == 0) THEN
            act = 'Read'
        ELSE
            act = 'Write'
        END IF

        CALL OpenAndIO(i, unitNum( i ), act, dataValue( i ), ids( i ))
        newIDs( i ) = MangleID( ids( i ) )
    END DO


    DO i = 10, 1, -1
        ioID = MangleID( newIDs( i ) )

        PRINT *, "ids(", i, ") =", ids( i ),&
                &", newIDs(", i, ") =", newIDs( i ), ", ioID =", ioID


        WAIT(unitNum( i ), ID=ioID, IOSTAT=iStat, IOMSG=iMsg)

        WRITE(0, *) "WAIT(", unitNum( i ), ") <", iStat, "> ", iMsg
        IF (iStat == 0) THEN
            CALL zzrc( (i + 20) )
        END IF


        CLOSE(unitNum( i ), IOSTAT=iStat, IOMSG=iMsg)
        IF (iStat /= 0) THEN
            WRITE(0, *) "CLOSE(", unitNum( i ), ") <", iStat, "> ", iMsg
            CALL zzrc( (i + 30) )
        END IF
    END DO


END PROGRAM idSpecIDValue04d


INTEGER FUNCTION MangleID( ioID )
    INTEGER, INTENT(IN) :: ioID

    MangleID = ((ioID + 100) / 3) * 2

END FUNCTION MangleID


SUBROUTINE OpenAndIO(i, ioUnit, act, dataValue, ioID)

    INTEGER, INTENT(IN) :: i
    INTEGER, INTENT(IN) :: ioUnit
    CHARACTER(LEN = 5), INTENT(IN) :: act
    INTEGER, ASYNCHRONOUS, INTENT(INOUT) :: dataValue
    INTEGER, ASYNCHRONOUS, INTENT(INOUT) :: ioID

    CHARACTER(LEN = 256) :: iMsg


    OPEN(ioUnit, ACTION=act, ASYNCHRONOUS='yes',&
        &ACCESS='stream', FORM='unformatted', IOSTAT=iStat, IOMSG=iMsg)

    IF (iStat /= 0) THEN
        WRITE(0, *) "OPEN(", ioUnit, ",", act, ") <", iStat, "> ", iMsg
        CALL zzrc( i )
    END IF


    IF (act == 'Read') THEN
        READ(ioUnit, ASYNCHRONOUS='yes',&
            &ID=ioID, IOSTAT=iStat, IOMSG=iMsg) dataValue

        IF (iStat /= 0) THEN
            WRITE(0, *) "READ(", ioUnit, ") <", iStat, "> ", iMsg
            CALL zzrc( (i + 10) )
        END IF

    ELSE
        WRITE(ioUnit, ASYNCHRONOUS='yes',&
            &ID=ioID, IOSTAT=iStat, IOMSG=iMsg) dataValue

        IF (iStat /= 0) THEN
            WRITE(0, *) "WRITE(", ioUnit, ") <", iStat, "> ", iMsg
            CALL zzrc( (i + 10) )
        END IF
    END IF

END SUBROUTINE OpenAndIO
