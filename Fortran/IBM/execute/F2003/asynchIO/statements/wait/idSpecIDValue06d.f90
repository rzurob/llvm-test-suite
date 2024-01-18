!*  ===================================================================
!*
!*  DATE                       : March  7, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() Statement
!*  SECONDARY FUNCTIONS TESTED : ID= Specifier where the Value is the same
!*                               for all WAIT() Statements
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

PROGRAM idSpecIDValue06d
    IMPLICIT NONE

    INTEGER :: i

    INTEGER, DIMENSION( 10 ) :: iID
    INTEGER, DIMENSION( 10 ) :: dataValue

    INTEGER :: iStat
    CHARACTER(LEN = 256) :: iMsg


    OPEN(ASYNCHRONOUS='yes', IOSTAT=iStat,&
        &UNIT=99, ACTION='read', ACCESS='sequential',&
        &IOMSG=iMsg, FILE='idSpecIDValue06d.dat', FORM='unformatted')
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    DO i = 1, 10
        READ(ASYNCHRONOUS='yes', UNIT=99, IOMSG=iMsg,&
                &ID=iID( i ), IOSTAT=iStat) dataValue( i )

        IF (iStat <> 0) THEN
            WRITE(0, *) "READ() <", iStat, "> ", iMsg
            CALL zzrc( (10 + i) )
        END IF
    END DO


    CALL Wait4It(99, iID)


    CLOSE(IOMSG=iMsg, UNIT=99, IOSTAT=iStat)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 30 )
    END IF


    PRINT *
    PRINT *, "Data Values Read:"
    DO i = 10, 1, -1
        PRINT *, "dataValue(", i, ") =", dataValue( i )
    END DO

END PROGRAM idSpecIDValue06d


SUBROUTINE Wait4It(ioUnit, iID)
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: ioUnit
    INTEGER, DIMENSION( 10 ), INTENT(IN) :: iID

    INTEGER :: i
    INTEGER :: iStat

    CHARACTER(LEN = 256) :: iMsg


    WAIT(UNIT=ioUnit, IOMSG=iMsg, ID=iID( 1 ), IOSTAT=iStat)
    IF (iStat <> 0) THEN
        WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
        CALL zzrc( 21 )
    END IF


    !
    !  "iID( 1 )" is no longer the Identifier for a Pending Data Transfer.
    !
    DO i = 2, 10
        WAIT(ioUnit, IOMSG=iMsg, ID=iID( 1 ), IOSTAT=iStat)
        WRITE(6, *) "WAIT() <", iStat, "> ", iMsg

        IF (iStat == 0) THEN
            CALL zzrc( (20 + i) )
        END IF
    END DO

END SUBROUTINE Wait4It
