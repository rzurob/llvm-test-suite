!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : asynchSpecOpen06d - ASYNCHRONOUS= Specifier
!*                               in I/O Statements
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : January 23, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : F2003 Standard:  ASYNCHRONOUS= Specifier in
!*                               OPEN() Statement
!*  SECONDARY FUNCTIONS TESTED : IBM Extension:  ASYNCH= Specifier in OPEN()
!*                               Statement
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : OPEN(), ASYNCHRONOUS= and ASYNCH= Specifiers
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 3
!*
!*  DESCRIPTION                :
!*  From the I0:
!*
!*  Constraints
!*
!*  For the 'ASYNCHRONOUS=' specifier:
!*
!*  o  For OPEN/INQUIRE statement: An 'ASYNCH=' specifier and an
!*     'ASYNCHRONOUS=' specifier should not appear on the same OPEN or
!*     INQUIRE statement. The second one is ignored.
!*
!*  From the F2003 Standard:
!*
!*  9.4.5 The OPEN statement
!*
!*  R904 open-stmt  is  OPEN ( connect-spec-list )
!*  R905 connect-spec  is  [ UNIT = ] file-unit-number
!*                     or ACCESS = scalar-default-char-expr
!*                     or ACTION = scalar-default-char-expr
!*                     or ASYNCHRONOUS = scalar-default-char-expr
!*  ...
!*
!*  C903 (R905) No specifier shall appear more than once in a given
!*              connect-spec-list.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM asynchSpecOpen06d
    IMPLICIT NONE

    INTEGER :: ioStatus

    CHARACTER(len = 3) :: asynchVar
    CHARACTER(len = 256) :: ioErrorMsg


    !
    !  Both ASYNCHRONOUS= and ASYNCH= Specifiers present (ASYNCH= Specifier
    !  second), ASYNCH= Specifier should be ignored.
    !
    OPEN(8, ASYNCHRONOUS='YES', ASYNCH='NO',&
                &IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "OPEN(): ", ioErrorMsg
        CALL zzrc( 1 )
    END IF


    INQUIRE(8, ASYNCHRONOUS=asynchVar, IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "INQUIRE(): (", ioStatus, ") ", ioErrorMsg
        CALL zzrc( 2 )
    END IF


    !
    !  INQUIRE() should return "YES".
    !
    IF (asynchVar /= 'YES') THEN
        PRINT *, "INQUIRE(): ASYNCHRONOUS=(", asynchVar, ")"
        CALL zzrc( 3 )
    END IF


    CLOSE(8, IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "CLOSE(): ", ioErrorMsg
        CALL zzrc( 4 )
    END IF


    !
    !  Both ASYNCHRONOUS= and ASYNCH= Specifiers present (ASYNCHRONOUS=
    !  Specifier second), ASYNCHRONOUS= Specifier should be ignored.
    !
    OPEN(8, ASYNCH='NO', ASYNCHRONOUS='YES',&
                &IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "OPEN(): ", ioErrorMsg
        CALL zzrc( 5 )
    END IF


    INQUIRE(8, ASYNCHRONOUS=asynchVar, IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "INQUIRE(): (", ioStatus, ") ", ioErrorMsg
        CALL zzrc( 6 )
    END IF


    !
    !  INQUIRE() should return "NO".
    !
    IF (asynchVar /= 'NO') THEN
        PRINT *, "INQUIRE(): ASYNCHRONOUS=(", asynchVar, ")"
        CALL zzrc( 7 )
    END IF


    CLOSE(8, IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "CLOSE(): ", ioErrorMsg
        CALL zzrc( 8 )
    END IF


    !
    !  Two instances of the ASYNCHRONOUS= Specifier, second ASYNCHRONOUS=
    !  Specifier should be ignored.
    !
    OPEN(8, ASYNCHRONOUS='no', ASYNCHRONOUS='NO',&
                    &IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "OPEN(): ", ioErrorMsg
        CALL zzrc( 9 )
    END IF


    INQUIRE(8, ASYNCHRONOUS=asynchVar, IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "INQUIRE(): (", ioStatus, ") ", ioErrorMsg
        CALL zzrc( 10 )
    END IF


    !
    !  INQUIRE() should return "NO".
    !
    IF (asynchVar /= 'NO') THEN
        PRINT *, "INQUIRE(): ASYNCHRONOUS=(", asynchVar, ")"
        CALL zzrc( 11 )
    END IF


    CLOSE(8, IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "CLOSE(): ", ioErrorMsg
        CALL zzrc( 12 )
    END IF


END PROGRAM asynchSpecOpen06d
