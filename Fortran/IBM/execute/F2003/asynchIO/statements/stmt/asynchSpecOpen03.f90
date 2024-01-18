!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : asynchSpecOpen03 - ASYNCHRONOUS= Specifier
!*                               in I/O Statements
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : January 23, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS= Specifier in OPEN() Statement
!*  SECONDARY FUNCTIONS TESTED : scalar-default-char-expr is 'YES' (with
!*                               trailing blanks), ASYNCHRONOUS I/O is allowed
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : OPEN(), ASYNCHRONOUS= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                :
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
!*  From Section 9.4.5 The OPEN statement (pg. 182 -- Second Paragraph):
!*
!*  A specifier that requires a scalar-default-char-expr may have a limited
!*  list of character values. These values are listed for each such specifier.
!*  Any trailing blanks are ignored. ...
!*
!*  9.4.5.3 ASYNCHRONOUS= specifier in the OPEN statement
!*
!*  The scalar-default-char-expr shall evaluate to YES or NO.  If YES is
!*  specified, asynchronous input/output on the unit is allowed.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM asynchSpecOpen03
    IMPLICIT NONE

    INTEGER :: i = 5
    INTEGER :: ioStatus

    CHARACTER(len = 3) :: asynchVar
    CHARACTER(len = 256) :: ioErrorMsg


    !
    !  (Secondary Test) The value for the ASYNCHRONOUS= Specifier contains
    !  trailing blanks.
    !
    OPEN(8, ACTION='WRITE', ASYNCHRONOUS='YES  ',&
                &IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "OPEN(): (", ioStatus, ") ", ioErrorMsg
        CALL zzrc( 1 )
    END IF


    !
    !  Asynchronous WRITE(), should be Successful.
    !
    WRITE(8, *, ASYNCHRONOUS='YES',&
                &IOSTAT=ioStatus, IOMSG=ioErrorMsg) i
    IF (ioStatus /= 0) THEN
        PRINT *, "WRITE(): (", ioStatus, ") ", ioErrorMsg
        CALL zzrc( 2 )
    END IF


    INQUIRE(8, ASYNCHRONOUS=asynchVar, IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "INQUIRE(): (", ioStatus, ") ", ioErrorMsg
        CALL zzrc( 3 )
    END IF


    !
    !  INQUIRE() should return "YES".
    !
    IF (asynchVar /= 'YES') THEN
        PRINT *, "INQUIRE(): ASYNCHRONOUS=(", asynchVar, ")"
        CALL zzrc( 4 )
    END IF


    CLOSE(8, IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "CLOSE(): (", ioStatus, ") ", ioErrorMsg
        CALL zzrc( 5 )
    END IF


END PROGRAM asynchSpecOpen03
