!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : asynchSpecOpen11d - ASYNCHRONOUS=
!*                               Specifier in I/O Statements
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : February  1, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS= Specifier in the OPEN()
!*                               Statement
!*  SECONDARY FUNCTIONS TESTED : File Previously OPEN()ed for non-Asynchronous
!*                               I/O
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
!*  9.4.1 Connection modes
!*
!*  A connection for formatted input/output has several changeable modes: the
!*  blank interpretation mode (10.7.6), delimiter mode (10.9.2, 10.10.2.1),
!*  sign mode (10.7.4), decimal edit mode (10.7.8), I/O rounding mode
!*  (10.6.1.2.6), pad mode (9.5.3.4.2), and scale factor (10.7.5). ...
!*
!*  Values for the modes of a connection are established when the connection
!*  is initiated. ...
!*
!*  The modes of a connection to an external file may be changed by a
!*  subsequent OPEN statement that modifies the connection.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM asynchSpecOpen11d
    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: ioStatus

    CHARACTER(len = 384) :: ioErrorMsg


    !
    !  OPEN() for Asynchronous Output.
    !
    OPEN(ACTION='read', ASYNCHRONOUS='no',&
         &IOSTAT=ioStatus, IOMSG=ioErrorMsg, UNIT=15)

    IF (ioStatus <> 0) THEN
        PRINT *, "OPEN(ASYNCHRONOUS='no'): ioStatus = (",&
                                            &ioStatus, ")", ioErrorMsg
        CALL zzrc( 1 )
    END IF


    !
    !  Re-OPEN() Non-Asynchronous Output.
    !
    OPEN(UNIT=15, ASYNCHRONOUS='yes', IOSTAT=ioStatus, IOMSG=ioErrorMsg)

    PRINT *, "OPEN(ASYNCHRONOUS='yes'): ioStatus = (",&
                                        &ioStatus, ") ", ioErrorMsg

    IF (ioStatus <> 24) THEN
        CALL zzrc( 2 )
    END IF


    !
    !  Re-OPEN() Unit 0 (STDERR) for Asynchronous Output.
    !
    CLOSE(IOSTAT=ioStatus, UNIT=15, IOMSG=ioErrorMsg)

    IF (ioStatus /= 0) THEN
        PRINT *, "CLOSE(): ioStatus = (", ioStatus, ") ", ioErrorMsg
        CALL zzrc( 3 )
    END IF


END PROGRAM asynchSpecOpen11d
