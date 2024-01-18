!*  ===================================================================
!*
!*                               Specifier in I/O Statements
!*
!*  DATE                       : March 17, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS= Specifier in INQUIRE()
!*                               Statement on a file OPEN()ed for both
!*                               Unormatted Input and Output
!*  SECONDARY FUNCTIONS TESTED : Value in scalar-default-char-variable is:
!*                               'YES', or 'NO'
!*
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

PROGRAM asynchSpecInquire09

    CHARACTER(LEN = 9) :: asynchType

    CHARACTER(LEN = 256) :: iMsg


    OPEN(2295, ACTION='readwrite', ASYNCHRONOUS='yes',&
            &FORM='unformatted', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN(ASYNCHRONOUS=yes) <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    OPEN(2771, ACTION='readwrite', ASYNCHRONOUS='no',&
            &FORM='unformatted', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN(ASYNCHRONOUS=no) <", iStat, "> ", iMsg
        CALL zzrc( 2 )
    END IF


    !
    !  1)  File Opened for Asynchronous Input/Output
    !
    INQUIRE(2295, ASYNCHRONOUS=asynchType, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "INQUIRE(ASYNCHRONOUS=yes) <", iStat, "> ", iMsg
        CALL zzrc( 3 )

    ELSE IF (asynchType <> 'YES') THEN
        WRITE(0, *) "INQUIRE(ASYNCHRONOUS=", asynchType, ")"
        WRITE(0, *) " ... expected 'YES'"

        CALL zzrc( 4 )
    END IF


    CLOSE(2295, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE(ASYNCHRONOUS=yes) <", iStat, "> ", iMsg
        CALL zzrc( 5 )
    END IF


    !
    !  2)  File Opened for Synchronous Input/Output
    !
    INQUIRE(2771, ASYNCHRONOUS=asynchType, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "INQUIRE(ASYNCHRONOUS=no) <", iStat, "> ", iMsg
        CALL zzrc( 6 )

    ELSE IF (asynchType <> 'NO') THEN
        WRITE(0, *) "INQUIRE(ASYNCHRONOUS=", asynchType, ")"
        WRITE(0, *) " ... expected 'NO'"

        CALL zzrc( 7 )
    END IF


    CLOSE(2771, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE(ASYNCHRONOUS=no) <", iStat, "> ", iMsg
        CALL zzrc( 8 )
    END IF

END PROGRAM asynchSpecInquire09
