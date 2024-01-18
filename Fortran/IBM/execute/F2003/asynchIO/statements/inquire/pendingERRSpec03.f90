!*  ===================================================================
!*
!*  DATE                       : March 24, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : An Error in one of several Pending Data
!*                               Transfers
!*  SECONDARY FUNCTIONS TESTED : INQUIRE() with the PENDING=, ID=, and
!*                               ERR= Specifiers
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ(), INQUIRE(), PENDING= Specifier,
!*                               ID= Specifier, ERR= Specifier
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
!*                     or  ERR = label
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

PROGRAM pendingERRSpec03

    INTEGER, DIMENSION( 5 ) :: aID
    INTEGER, DIMENSION( 5 ) :: dataIn

    INTEGER, DIMENSION( 5 ) :: recNum = (/ 2, 4, 66, 8, 10 /)

    INTEGER, DIMENSION( 10 ) :: dataOut

    LOGICAL :: pFending

    CHARACTER(LEN = 256) :: iMsg


    OPEN(217, ASYNCHRONOUS='yes', ACTION='readwrite', RECL=4,&
        FORM='unformatted', ACCESS='direct', IOSTAT=iStat, IOMSG=iMsg)
    IF (0 /= iStat) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    CALL WriteData(217, dataOut)


    dataIn = 0
    DO i = 5, 1, -1
        READ(217, ASYNCHRONOUS='yes', ID=aID( i ),&
            REC=recNum( i ), IOSTAT=iStat, IOMSG=iMsg) dataIn( i )

        IF (0 /= iStat) THEN
            WRITE(0, *) i, "READ() <", iStat, "> ", iMsg
            CALL zzrc( (20 + i) )
        END IF
    END DO


    PRINT "('dataOut(',10I6,')')", (dataOut( i ), i = 1, 10)


    DO i = 1, 5
        INQUIRE(217, ID=aID( i ), ERR=100,&
            PENDING=pFending, IOSTAT=iStat, IOMSG=iMsg)

        IF ((i /= 3)  .AND.  (iStat /= 0)) THEN
            WRITE(0, *) i, "INQUIRE(ID=", aID( i ), ") <", iStat, "> ", iMsg
            CALL zzrc( (30 + i) )
        END IF

        GOTO 200

100     WRITE(6, *) i, "INQUIRE(ERR=100,ID=",&
                    aID( i ), ") <", iStat, "> ", iMsg

200     CONTINUE
    END DO


    CLOSE(217, IOSTAT=iStat, IOMSG=iMsg)
    IF (0 /= iStat) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 51 )
    END IF


    PRINT "(' dataIn(',5I6,')')", (dataIn( i ), i = 1, 5)


    DO i = 1, 5
        IF (i /= 3) THEN
            j = i * 2

            IF (dataOut( j ) /= dataIn( i )) THEN
                WRITE(6, *) " dataIn(", i, ") = '", dataIn( i ), "'"
                WRITE(6, *) "dataOut(", j, ") = '", dataOut( j ), "'"

                CALL zzrc( 60 + i )
            END IF
        END IF
    END DO

END PROGRAM pendingERRSpec03


SUBROUTINE WriteData(ioUnit, dataValues)

    INTEGER, INTENT(IN) :: ioUnit
    INTEGER, DIMENSION( 10 ), INTENT(OUT) :: dataValues

    CHARACTER(LEN = 256) :: iMsg


    DO i = 10, 1, -1
        dataValues( i ) = (i * 1292) + 29

        WRITE(217, ASYNCHRONOUS='no', REC=i,&
            &IOSTAT=iStat, IOMSG=iMsg) dataValues( i )

        IF (0 /= iStat) THEN
            WRITE(0, *) i, "WRITE() <", iStat, "> ", iMsg
            CALL zzrc( (10 + i) )
        END IF
    END DO

END SUBROUTINE WriteData
