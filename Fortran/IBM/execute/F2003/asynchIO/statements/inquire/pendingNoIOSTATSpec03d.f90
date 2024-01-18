!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : pendingNoIOSTATSpec03d - INQUIRE() Statement
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : March 29, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pending Data Transfers with an Error Condition
!*  SECONDARY FUNCTIONS TESTED : INQUIRE() with the ID=, and PENDING=
!*                               Specifiers; the ERR= and IOSTAT= Specifiers
!*                               are *NOT* present
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : INQUIRE(), ID= Specifier, PENDING= Specifier
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
!*                     or  ERR = label
!*  ...
!*                     or  IOSTAT = scalar-int-variable
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

PROGRAM pendingNoIOSTATSpec03d

    INTEGER, DIMENSION( 10 ) :: ioID
    INTEGER, DIMENSION( 10 ) :: dataValue

    LOGICAL :: independent

    CHARACTER(LEN = 256) :: iMsg


    OPEN(45, ACCESS='direct', RECL=4, ACTION='read',&
        &FORM='unformatted', ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 11 )
    END IF


    DO i = 0, 9
        READ(45, REC=(i + 1), ASYNCHRONOUS='yes', IOMSG=iMsg,&
            ID=ioID( (i + 1) ), IOSTAT=iStat) dataValue( (i + 1) )

        IF (iStat /= 0) THEN
            WRITE(0, *) i, "READ() <", iStat, "> ", iMsg
            CALL zzrc( (20 + i) )
        END IF
    END DO


    WRITE(6, '(10I3)') (ioID( i ), i = 1, 10)


    DO i = 1, 10
        WRITE(0, *) "ioID(", i, ") = '", ioID( i ), "'"

        INQUIRE(45, ID=ioID( i ), PENDING=independent)
    END DO


    CLOSE(45, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 41 )
    END IF

END PROGRAM pendingNoIOSTATSpec03d
