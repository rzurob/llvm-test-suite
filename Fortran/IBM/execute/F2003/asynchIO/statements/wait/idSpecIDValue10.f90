!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : idSpecIDValue10 - WAIT() Statement
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : March 15, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() Statement on (Formatted) I/O Units
!*  SECONDARY FUNCTIONS TESTED : WAIT() on an ID= Value that no longer is
!*                               associated with a Pending Data Transfer
!*
!*  DRIVER STANZA              : xlf2003
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

PROGRAM idSpecIDValue10

    INTEGER, DIMENSION( 10 ) :: aID
    INTEGER, DIMENSION( 10 ) :: inData
    INTEGER, DIMENSION( 10 ) :: outData

    CHARACTER(LEN = 256) :: iMsg


    OPEN(UNIT=436, ACTION='write', ACCESS='sequential',&
        &FORM='formatted', ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN(Write) <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    outData = (/ ((i * 456), i = 1, 10) /)

    DO i = 1, 10
        WRITE(436, '(I5)', ASYNCHRONOUS='yes',&
            &ID=aID( i ), IOSTAT=iStat, IOMSG=iMsg) outData( i )

        IF (iStat <> 0) THEN
            WRITE(0, *) i, ") WRITE() <", iStat, "> ", iMsg
            CALL zzrc( (10 + i) )
        END IF
    END DO


    CALL idWait(436, aID, 20)
    CALL idWait(436, aID, 30)


    CLOSE(UNIT=436, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE(Write) <", iStat, "> ", iMsg
        CALL zzrc( 41 )
    END IF


    OPEN(UNIT=436, ACTION='read', ACCESS='sequential',&
        &FORM='formatted', ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN(Read) <", iStat, "> ", iMsg
        CALL zzrc( 51 )
    END IF


    DO i = 1, 10
        READ(436, '(I5)', ASYNCHRONOUS='yes',&
            &ID=aID( i ), IOSTAT=iStat, IOMSG=iMsg) inData( i )

        IF (iStat <> 0) THEN
            WRITE(0, *) i, ") WRITE() <", iStat, "> ", iMsg
            CALL zzrc( (60 + i) )
        END IF
    END DO


    CALL idWait(436, aID, 70)
    CALL idWait(436, aID, 80)


    CLOSE(UNIT=436, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE(Read) <", iStat, "> ", iMsg
        CALL zzrc( 91 )
    END IF


    DO i = 1, 10
        IF (outData( i ) <> inData( i )) THEN
            WRITE(0, *) " inData(", i, ") = '", inData( i ), "'"
            WRITE(0, *) "outData(", i, ") = '", outData( i ), "'"
            CALL zzrc( (100 + i) )
        END IF
    END DO

END PROGRAM idSpecIDValue10


SUBROUTINE idWait(ioUnit, ioID, failRC)

    INTEGER :: ioUnit
    INTEGER, DIMENSION( 10 ) :: ioID
    INTEGER :: failRC

    CHARACTER(LEN = 256) :: iMsg


    DO i = 1, 10
        WAIT(ioUnit, ID=ioID( i ), IOSTAT=iStat, IOMSG=iMsg)

        IF (iStat <> 0) THEN
            WRITE(0, *) i, ") WAIT(", ioID( i ), ") <", iStat, "> ", iMsg
            CALL zzrc( (failRC + i) )
        END IF
    END DO

END SUBROUTINE idWait
