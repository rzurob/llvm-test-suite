!*  ===================================================================
!*
!*                               in I/O Statements
!*
!*  DATE                       : January 23, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS= Specifier in OPEN() Statement
!*  SECONDARY FUNCTIONS TESTED : scalar-default-char-expr is neither 'YES'
!*                               nor 'NO'
!*
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
!*  The scalar-default-char-expr shall evaluate to YES or NO.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM asynchSpecOpen02d
    IMPLICIT NONE

    INTEGER :: ioStatus
    CHARACTER(len = 256) :: ioErrorMsg


    OPEN(8, ASYNCHRONOUS='MAYBE', IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "OPEN(): ", ioErrorMsg
        CALL zzrc( 1 )
    END IF


    CLOSE(8, IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "CLOSE(): ", ioErrorMsg
        CALL zzrc( 2 )
    END IF


END PROGRAM asynchSpecOpen02d