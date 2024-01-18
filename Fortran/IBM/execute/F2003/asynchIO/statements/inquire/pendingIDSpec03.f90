!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : pendingIDSpec03 - INQUIRE() Statement
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : March 20, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Perform Unformatted Asynchronous I/O
!*                               Operations on several Units
!*  SECONDARY FUNCTIONS TESTED : Perform an INQUIRE() on each Pending
!*                               Data Transfer on a Unit other than the
!*                               Unit for which the Pending Data Transfer
!*                               was initiated
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ(), WRITE(), INQUIRE(), PENDING=
!*                               Specifier, ID= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
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
!*
!*  9.9.1.13 ID= specifier in the INQUIRE statement
!*
!*  The value of the expression specified in the ID= specifier shall be the
!*  identifier of a pending data transfer operation for the specified unit.
!*
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

PROGRAM pendingIDSpec03

    INTEGER, DIMENSION( 10,100 ) :: ioID
    INTEGER, DIMENSION( 10,100 ) :: dataValues

    LOGICAL :: isPending

    INTEGER, DIMENSION( 10 ) :: ioUnit = (/ (i, i = 11, 20) /)

    CHARACTER(LEN = 5), DIMENSION( 2 ) :: actType = (/ 'read ', 'write' /)

    CHARACTER(LEN = 256) :: iMsg


    DO i = 1, 10
        j = MOD(i, 2) + 1
        OPEN(ioUnit( i ), ASYNCHRONOUS='yes',&
            &IOSTAT=iStat, FORM='unformatted',&
            &ACTION=actType( j ), ACCESS='sequential', IOMSG=iMsg)

        IF (iStat /= 0) THEN
            WRITE(0, *) "OPEN(", ioUnit( i ), ",",&
                        &actType( j ), ") <", iStat, "> ", iMsg
            CALL zzrc( i )
        END IF
    END DO


    dataValues =&
        &RESHAPE((/ ((((i * 123) + 789), j = 1, 10), i = 1, 100) /),&
                                                        &(/ 10,100 /))


    DO i = 1, 100
        DO j = 1, 10
            IF (MOD(j, 2) == 0) THEN
                READ(ioUnit( j ), ASYNCHRONOUS='yes', IOMSG=iMsg,&
                    &ID=ioID( j,i ), IOSTAT=iStat) dataValues( j,i )

                IF (iStat /= 0) THEN
                    WRITE(0, *) "READ(", ioUnit( j ),&
                                    &") <", iStat, "> ", iMsg
                    CALL zzrc( (10 + j) )
                END IF

            ELSE
                WRITE(ioUnit( j ), ASYNCHRONOUS='yes', IOMSG=iMsg,&
                    &ID=ioID( j,i ), IOSTAT=iStat) dataValues( j,i )

                IF (iStat /= 0) THEN
                    WRITE(0, *) "WRITE(", ioUnit( j ),&
                                    &") <", iStat, "> ", iMsg
                    CALL zzrc( (10 + j) )
                END IF
            END IF
        END DO
    END DO


    isPending = .FALSE.
    DO i = 10, 1, -1
        k = 10 - i + 1

        DO j = 100, 1, -1
            iMsg = ''

            INQUIRE(ioUnit( i ), ID=ioID( k,j ),&
                    &PENDING=isPending, IOSTAT=iStat, IOMSG=iMsg)

            IF (iStat /= 227) THEN
                WRITE(0, *) "INQUIRE(", ioUnit( i ), ",ID=",&
                                    &ioID( k,j ), ") <", iStat, "> ", iMsg
                CALL zzrc( (i + 20) )

            ELSE IF ( isPending ) THEN
                WRITE(0, *) "INQUIRE(", ioUnit( i ), ",", isPending, ")"
                CALL zzrc( (i + 30) )
            END IF
        END DO
    END DO


    DO i = 1, 10
        CLOSE(ioUnit( i ), IOSTAT=iStat, IOMSG=iMsg)
        IF (iStat /= 0) THEN
            WRITE(0, *) "CLOSE(", ioUnit( i ), ") <", iStat, "> ", iMsg
            CALL zzrc( (i + 40) )
        END IF
    END DO


    DO i = 1, 100
        WRITE(6, '(5I6)') (dataValues( j,i ), j = 1, 9, 2)
    END DO

END PROGRAM pendingIDSpec03
