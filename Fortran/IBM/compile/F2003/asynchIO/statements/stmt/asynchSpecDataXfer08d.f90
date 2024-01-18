!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : asynchSpecDataXfer08d - ASYNCHRONOUS=
!*                               Specifier in I/O Statements
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : March  1, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS=variable Specifier in Data
!*                               Transfer Statement
!*  SECONDARY FUNCTIONS TESTED : File OPEN()ed with ASYNCHRONOUS=variable
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ(), WRITE(), ASYNCHRONOUS= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 3
!*
!*  DESCRIPTION                :
!*  Diagnostic Test Case to confirm that the ASYNCHRONOUS= Specifier use
!*  in the WRITE() and READ() Statements should have a Value that is a
!*  scalar-char-initialization-expr.
!*
!*  9.5.1 Control information list
!*
!*  R913 io-control-spec  is  [ UNIT = ] io-unit
!*                        or  [ FMT = ] format
!*                        or  [ NML = ] namelist-group-name
!*                        or  ADVANCE = scalar-default-char-expr
!*                        or  ASYNCHRONOUS = scalar-char-initialization-expr
!*  ...
!*
!*  C924 (R913) The scalar-char-initialization-expr in an ASYNCHRONOUS=
!*              specifier shall be of type default character and shall
!*              have the value YES or NO.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM asynchSpecDataXfer08d
    IMPLICIT NONE

    INTEGER :: ioStatus

    REAL :: in
    REAL :: out = 1.234

    CHARACTER(LEN = 3) :: asynchFlag = 'yes'
    CHARACTER(LEN = 256) :: ioMessage


    OPEN(101, ASYNCHRONOUS=asynchFlag,&
        &ACCESS='stream', FORM='unformatted',&
        &ACTION='readwrite', IOSTAT=ioStatus, IOMSG=ioMessage)
    IF (ioStatus <> 0) THEN
        WRITE(0, *) "OPEN() <", ioStatus, "> ", ioMessage
        CALL zzrc( 1 )
    END IF


    WRITE(101, ASYNCHRONOUS=asynchFlag,&
        &IOSTAT=ioStatus, IOMSG=ioMessage) out
    IF (ioStatus <> 0) THEN
        WRITE(0, *) "WRITE() <", ioStatus, "> ", ioMessage
        CALL zzrc( 2 )
    END IF


    WAIT(101, IOSTAT=ioStatus, IOMSG=ioMessage)
    IF (ioStatus <> 0) THEN
        WRITE(0, *) "WAIT(Write) <", ioStatus, "> ", ioMessage
        CALL zzrc( 3 )
    END IF


    REWIND 101


    READ(101, ASYNCHRONOUS=asynchFlag,&
        &IOSTAT=ioStatus, IOMSG=ioMessage) in
    IF (ioStatus <> 0) THEN
        WRITE(0, *) "READ() <", ioStatus, "> ", ioMessage
        CALL zzrc( 4 )
    END IF


    WAIT(101, IOSTAT=ioStatus, IOMSG=ioMessage)
    IF (ioStatus <> 0) THEN
        WRITE(0, *) "WAIT(Read) <", ioStatus, "> ", ioMessage
        CALL zzrc( 5 )
    END IF


    CLOSE(101, IOSTAT=ioStatus, IOMSG=ioMessage)
    IF (ioStatus <> 0) THEN
        WRITE(0, *) "CLOSE() <", ioStatus, "> ", ioMessage
        CALL zzrc( 6 )
    END IF


    IF (out .NE. in) THEN
        WRITE(0, *) " in = '", in, "'"
        WRITE(0, *) "out = '", in, "'"
        CALL zzrc( 7 )
    END IF

END PROGRAM asynchSpecDataXfer08d
