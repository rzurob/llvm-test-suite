!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : asynchSpecOpen05d - ASYNCHRONOUS= Specifier
!*                               in I/O Statements
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : January 23, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : No ASYNCHRONOUS= Specifier in OPEN() Statement
!*  SECONDARY FUNCTIONS TESTED : Default to ASYNCHRONOUS='NO'
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : OPEN(), ASYNCHRONOUS= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
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
!*  9.4.5.3 ASYNCHRONOUS= specifier in the OPEN statement
!*
!*  The scalar-default-char-expr shall evaluate to YES or NO.  ...
!*  If this specifier is omitted, the default value is NO.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM asynchSpecOpen05d
    IMPLICIT NONE

    INTEGER :: i = 5
    INTEGER :: ioStatus

    CHARACTER(len = 3) :: asynchVar
    CHARACTER(len = 256) :: ioErrorMsg


    !
    !  The ASYNCHRONOUS= Specifier is not present, OPEN() should default
    !  to non-Asynchronous I/O.
    !
    OPEN(8, ACTION='WRITE', IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "OPEN(): (", ioStatus, ") ", ioErrorMsg
        CALL zzrc( 1 )
    END IF


    !
    !  Non-Asynchronous WRITE(), should be Successful.
    !
    WRITE(8, *, IOSTAT=ioStatus, IOMSG=ioErrorMsg) i
    IF (ioStatus /= 0) THEN
        PRINT *, "WRITE(): (", ioStatus, ") ", ioErrorMsg
        CALL zzrc( 2 )
    END IF


    !
    !  Asynchronous WRITE(), should Fail.
    !
    WRITE(8, *, ASYNCHRONOUS='YES',&
                &IOSTAT=ioStatus, IOMSG=ioErrorMsg) i

    PRINT *, "WRITE(): (", ioStatus, ") ", ioErrorMsg
    IF (ioStatus /= 169) THEN
        CALL zzrc( 3 )
    END IF


    INQUIRE(8, ASYNCHRONOUS=asynchVar, IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "INQUIRE(): (", ioStatus, ") ", ioErrorMsg
        CALL zzrc( 3 )
    END IF


    !
    !  INQUIRE() should return "NO".
    !
    IF (asynchVar /= 'NO') THEN
        PRINT *, "INQUIRE(): ASYNCHRONOUS=(", asynchVar, ")"
        CALL zzrc( 4 )
    END IF


    CLOSE(8, IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "CLOSE(): (", ioStatus, ") ", ioErrorMsg
        CALL zzrc( 5 )
    END IF


END PROGRAM asynchSpecOpen05d
