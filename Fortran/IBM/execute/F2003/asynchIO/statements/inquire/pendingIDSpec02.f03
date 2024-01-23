!*  ===================================================================
!*
!*  DATE                       : March 17, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Perform Unformatted Asynchronous READ()
!*                               Operations
!*  SECONDARY FUNCTIONS TESTED : Perform an INQUIRE() on each Pending Data
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
!*  If an ID= specifier appears and the specified data transfer operation
!*  is complete, then the variable specified in the PENDING= specifier is
!*  assigned the value false and the INQUIRE statement performs the wait
!*  operation for the specified data transfer.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM pendingIDSpec02

    INTEGER, DIMENSION( 1000 ) :: ioID
    INTEGER, DIMENSION( 1000 ) :: dataValues

    LOGICAL, DIMENSION( 1000 ) :: depending

    CHARACTER(LEN = 256) :: iMsg


    ioUnit = 2223
    OPEN(ioUnit, ACCESS='sequential', ACTION='read',&
        &FILE='pendingIDSpec02.dat', FORM='unformatted',&
            &ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=iMsg)
    IF (0 /= iStat) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        ERROR STOP 1
    END IF


    DO i = 1, 1000
        READ(ioUnit, ASYNCHRONOUS='yes',&
            &ID=ioID( i ), IOSTAT=iStat, IOMSG=iMsg) dataValues( i )

        IF (0 /= iStat) THEN
            WRITE(0, *) i, "READ() <", iStat, "> ", iMsg
            ERROR STOP 2
        END IF
    END DO


    DO i = 1000, 1, -1
        INQUIRE(ioUnit, PENDING=depending( i ),&
            &ID=ioID( i ), IOSTAT=iStat, IOMSG=iMsg)

        IF (0 /= iStat) THEN
            WRITE(0, *) i, "INQUIRE(", ioID( i ), ") <", iStat, "> ", iMsg
            ERROR STOP 2
        END IF
    END DO


    DO i = 1, 1000
        WAIT(ioUnit, ID=ioID( i ), IOSTAT=iStat, IOMSG=iMsg)
        IF ( depending( i ) ) THEN
            IF (0 /= iStat) THEN
                WRITE(0, *) i, "WAIT(", ioID( i ), ") <", iStat, "> ", iMsg
                ERROR STOP 3
            END IF

        ELSE
            IF (224 /= iStat) THEN
                WRITE(0, *) i, "WAIT(", ioID( i ), ") <", iStat, "> ", iMsg
                ERROR STOP 4
            END IF
        END IF
    END DO


    DO i = 1, 1000, 10
        WRITE(6, '(10I5)') (dataValues( j ), j = i, (i + 9))
    END DO


    CLOSE(ioUnit, STATUS='keep', IOSTAT=iStat, IOMSG=iMsg)
    IF (0 /= iStat) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        ERROR STOP 5
    END IF

END PROGRAM pendingIDSpec02
