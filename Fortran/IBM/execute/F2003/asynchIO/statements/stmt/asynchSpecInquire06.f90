!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : asynchSpecInquire06 - ASYNCHRONOUS=
!*                               Specifier in I/O Statements
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : March 17, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS= Specifier in INQUIRE()
!*                               Statement on a file OPEN()ed for both
!*                               Formatted Input and Output
!*  SECONDARY FUNCTIONS TESTED : Value in scalar-default-char-variable is:
!*                               'YES', or 'NO'
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : INQUIRE(), ASYNCHRONOUS= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                :
!*
!*  9.9 File inquiry
!*
!*  R929 inquire-stmt  is  INQUIRE ( inquire-spec-list )
!*                     or  INQUIRE ( IOLENGTH = scalar-int-variable )&
!*                                   &output-item-list
!*
!*  9.9.1 Inquiry specifiers
!*
!*  R930 inquire-spec  is  [ UNIT = ] file-unit-number
!*                     or  FILE = file-name-expr
!*                     or  ACCESS = scalar-default-char-variable
!*                     or  ACTION = scalar-default-char-variable
!*                     or  ASYNCHRONOUS = scalar-default-char-variable
!*  ...
!*
!*  9.9.1.4 ASYNCHRONOUS= specifier in the INQUIRE statement
!*
!*  The scalar-default-char-variable in the ASYNCHRONOUS= specifier is
!*  assigned the value YES if the file is connected and asynchronous
!*  input/output on the unit is allowed; it is assigned the value NO if
!*  the file is connected and asynchronous input/output on the unit is
!*  not allowed. If there is no connection, the scalar-default-char-variable
!*  is assigned the value UNDEFINED.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM asynchSpecInquire06

    CHARACTER(LEN = 9) :: asynchType

    CHARACTER(LEN = 256) :: iMsg


    OPEN(9914, ASYNCHRONOUS='yes', FORM='formatted',&
        ACTION='readwrite', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN(AYNCHRONOUS=yes) <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    !
    ! 1)  ASYNCHRONOUS= Specifier in INQUIRE() Statement on a file
    !     OPEN()ed for both Unformatted Input and Output where the
    !     Value is 'YES'.
    !
    INQUIRE(UNIT=9914, ASYNCHRONOUS=asynchType, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "INQUIRE(AYNCHRONOUS=yes) <", iStat, "> ", iMsg
        CALL zzrc( 2 )

    ELSE IF (asynchType <> 'YES') THEN
        WRITE(0, *) "INQUIRE(AYNCHRONOUS=", asynchType, ")"
        WRITE(0, *) " ... Should be 'YES'"
        CALL zzrc( 3 )
    END IF


    CLOSE(9914, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE(AYNCHRONOUS=yes) <", iStat, "> ", iMsg
        CALL zzrc( 4 )
    END IF


    OPEN(9914, FORM='formatted', ACTION='readwrite', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN(AYNCHRONOUS=no) <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    !
    ! 2)  ASYNCHRONOUS= Specifier in INQUIRE() Statement on a file
    !     OPEN()ed for both Unformatted Input and Output where the
    !     Value is 'NO'.
    !
    INQUIRE(UNIT=9914, ASYNCHRONOUS=asynchType, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "INQUIRE(AYNCHRONOUS=no) <", iStat, "> ", iMsg
        CALL zzrc( 2 )

    ELSE IF (asynchType <> 'NO') THEN
        WRITE(0, *) "INQUIRE(AYNCHRONOUS=", asynchType, ")"
        WRITE(0, *) " ... Should be 'NO'"
        CALL zzrc( 3 )
    END IF


    CLOSE(9914, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE(AYNCHRONOUS=no) <", iStat, "> ", iMsg
        CALL zzrc( 4 )
    END IF

END PROGRAM asynchSpecInquire06
