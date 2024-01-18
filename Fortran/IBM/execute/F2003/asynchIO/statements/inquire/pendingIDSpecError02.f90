!*  ===================================================================
!*
!*  DATE                       : March 22, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : INQUIRE() with both the PENDING= and ID=
!*                               Specifiers
!*  SECONDARY FUNCTIONS TESTED : An Error Occurs on the second Pending Data
!*                               Transfer
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ(), INQUIRE(), PENDING= Specifier,
!*                               ID= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  9.6.2 Wait operation
!*
!*  If an error or end-of-file condition occurs during a wait operation for
!*  a unit, the processor performs a wait operation for all pending data
!*  transfer operations for that unit.
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
!*                     or  ID = scalar-int-expr
!*  ...
!*                     or  PENDING = scalar-default-logical-variable
!*  ...
!*
!*  C950 (R930) If an ID= specifier appears, a PENDING= specifier shall
!*              also appear.
!*
!*  9.9.1.13 ID= specifier in the INQUIRE statement
!*
!*  The value of the expression specified in the ID= specifier shall be the
!*  identifier of a pending data transfer operation for the specified unit.
!*
!*  9.9.1.20 PENDING= specifier in the INQUIRE statement
!*
!*  The PENDING= specifier is used to determine whether or not previously
!*  pending asynchronous data transfers are complete. A data transfer
!*  operation is previously pending if it is pending at the beginning of
!*  execution of the INQUIRE statement.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM pendingIDSpecError02

    INTEGER, DIMENSION( 3 ) :: ioID
    INTEGER, DIMENSION( 3 ) :: dataIn
    INTEGER, DIMENSION( 3 ) :: dataOut

    INTEGER, DIMENSION( 3 ) :: recNum = (/ 1, 22, 3 /)

    LOGICAL :: engPing
    LOGICAL :: pynding

    CHARACTER(LEN = 256) :: iMsg


    OPEN(323, FORM='unformatted', ACCESS='direct',RECL=4,&
        &ACTION='readwrite', ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    DO i = 1, 3
        dataOut( i ) = (i * 323) + 626

        WRITE(323, ASYNCHRONOUS='yes', REC=i,&
            &IOSTAT=iStat, IOMSG=iMsg) dataOut( i )

        IF (iStat /= 0) THEN
            WRITE(0, *) "WRITE() <", iStat, "> ", iMsg
            CALL zzrc( 10 + i )
        END IF
    END DO


    WAIT(323, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
        CALL zzrc( 21 )
    END IF


    DO i = 3, 1, -1
        READ(323, ASYNCHRONOUS='yes', REC=recNum( i ),&
            &ID=ioID( i ), IOSTAT=iStat, IOMSG=iMsg) dataIn( i )

        IF (iStat /= 0) THEN
            WRITE(0, *) "READ() <", iStat, "> ", iMsg
            CALL zzrc( 30 + i )
        END IF
    END DO


    INQUIRE(323, PENDING=engPing, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 1) THEN
        WRITE(0, *) "INQUIRE() <", iStat, "> ", iMsg
        CALL zzrc( 41 )
    END IF


    DO i = 1, 3
        INQUIRE(323, ID=ioID( i ), PENDING=pynding, IOSTAT=iStat, IOMSG=iMsg)

        IF ( engPing ) THEN
            IF (iStat /= 0) THEN
                WRITE(0, *) "INQUIRE(ID=", ioID( i ), ") <", iStat, "> ", iMsg
                CALL zzrc( 50 + i )
            END IF

        ELSE
            IF (iStat /= 226) THEN
                WRITE(0, *) "INQUIRE(ID=", ioID( i ), ") <", iStat, "> ", iMsg
                CALL zzrc( 60 + i )

            ELSE IF ( pynding ) THEN
                WRITE(0, *) "INQUIRE(ID=", ioID( i ), ",PENDING=", pynding, ")"
                CALL zzrc( 70 + i )

            ELSE IF ((i /= 2)  .AND.  (dataOut( i ) /= dataIn( i ))) THEN
                WRITE(0, *) " dataIn(", i, ") = '", dataIn( i ), "'"
                WRITE(0, *) "dataOut(", i, ") = '", dataOut( i ), "'"
                CALL zzrc( 80 + i )
            END IF
        END IF
    END DO


    CLOSE(323, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 91 )
    END IF


    PRINT '(3I5)', dataOut( 1 ), dataOut( 2 ), dataOut( 3 )
    PRINT '(I5,I5,I5)', dataIn( 1 ), dataIn( 2 ), dataIn( 3 )

END PROGRAM pendingIDSpecError02
