!*  ===================================================================
!*
!*  DATE                       : March 24, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pending Data Transfers
!*  SECONDARY FUNCTIONS TESTED : INQUIRE() with the NEXTREC= Specifier
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ(), INQUIRE(), NEXTREC= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  9.9 File inquiry
!*
!*  R929 inquire-stmt  is  INQUIRE ( inquire-spec-list )
!*
!*  9.9.1 Inquiry specifiers
!*
!*  R930 inquire-spec  is  [ UNIT = ] file-unit-number
!*                     or  FILE = file-name-expr
!*                     or  ACCESS = scalar-default-char-variable
!*                     or  ACTION = scalar-default-char-variable
!*                     or  ASYNCHRONOUS = scalar-default-char-variable
!*  ...
!*                     or  NEXTREC = scalar-int-variable
!*  ...
!*
!*  9.9.1.16 NEXTREC= specifier in the INQUIRE statement
!*
!*  The scalar-int-variable in the NEXTREC= specifier is assigned the value
!*  n + 1, where n is the record number of the last record read from or
!*  written to the file connected for direct access. ... If there are
!*  pending data transfer operations for the specified unit, the value
!*  assigned is computed as if all the pending data transfers had already
!*  completed.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM pendingNextRecSpec01

    INTERFACE
        INTEGER FUNCTION InquireAndWait(ioUnit, failRCbase)
            INTEGER, INTENT(IN) :: ioUnit
            INTEGER, INTENT(IN) :: failRCbase
        END FUNCTION InquireAndWait
    END INTERFACE

    INTEGER, DIMENSION( 10 ) :: dataIn
    INTEGER, DIMENSION( 10 ) :: dataOut

    INTEGER, DIMENSION( 10 ) :: recNum = (/ 5, 10, 7, 2, 1, 9, 3, 4, 8, 6 /)

    CHARACTER(LEN = 256) :: iMsg


    OPEN(213, ASYNCHRONOUS='yes', ACCESS='direct', IOMSG=iMsg,&
        FORM='unformatted', RECL=4, ACTION='readwrite', IOSTAT=iStat)
    IF (iStat /= 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        ERROR STOP 1
    END IF


    DO i = 1, 10
        dataOut( i ) = (562 * i) + 19

        WRITE(213, ASYNCHRONOUS='yes', IOMSG=iMsg,&
            &REC=recNum( i ), IOSTAT=iStat) dataOut( i )

        IF (iStat /= 0) THEN
            WRITE(0, *) i, "WRITE() <", iStat, "> ", iMsg
            CALL zzrc( 10 + i )
        END IF
    END DO


    nxtRec = InquireAndWait(213, 20)
    IF (nxtRec /= 7) THEN
        WRITE(0, *) "NEXTREC=", nxtRec, ", should be 7"
        ERROR STOP 31
    END IF

    PRINT '(10I5)', (dataOut( i ), i = 1, 10)


    DO i = 10, 1, -1
        READ(213, ASYNCHRONOUS='yes', IOMSG=iMsg,&
            &REC=recNum( i ), IOSTAT=iStat) dataIn( i )

        IF (iStat /= 0) THEN
            WRITE(0, *) i, "READ() <", iStat, "> ", iMsg
            CALL zzrc( 41 + i )
        END IF
    END DO


    nxtRec = InquireAndWait(213, 50)
    IF (nxtRec /= 6) THEN
        WRITE(0, *) "NEXTREC=", nxtRec, ", should be 6"
        ERROR STOP 61
    END IF

    PRINT '(10I5)', (dataIn( i ), i = 10, 1, -1)


    CLOSE(213, IOMSG=iMsg, IOSTAT=iStat)
    IF (iStat /= 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        ERROR STOP 71
    END IF


    DO i = 1, 10
        IF (dataOut( i ) /= dataIn( i )) THEN
            WRITE(6, *) " dataIn(", i, ") = ", dataIn( i )
            WRITE(6, *) "dataOut(", i, ") = ", dataOut( i )
            ERROR STOP 81
        END IF
    END DO

END PROGRAM pendingNextRecSpec01


INTEGER FUNCTION InquireAndWait(ioUnit, failRCbase)

    INTEGER, INTENT(IN) :: ioUnit
    INTEGER, INTENT(IN) :: failRCbase

    LOGICAL :: tripEnding

    CHARACTER(LEN = 256) :: iMsg


    INQUIRE(ioUnit, NEXTREC=nxtRec, IOMSG=iMsg, IOSTAT=iStat)
    IF (iStat /= 0) THEN
        WRITE(0, *) "INQUIRE() <", iStat, "> ", iMsg
        CALL zzrc( failRCbase + 1 )
    END IF


    PRINT *, "INQUIRE(NEXTREC=", nxtRec, ")"


    INQUIRE(ioUnit, PENDING=tripEnding, IOMSG=iMsg, IOSTAT=iStat)
    IF (iStat /= 0) THEN
        WRITE(0, *) "INQUIRE() <", iStat, "> ", iMsg
        CALL zzrc( failRCbase + 2 )
    END IF


    InquireAndWait = nxtRec

END FUNCTION InquireAndWait
