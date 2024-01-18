!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : asynchSpecDataXfer02d - ASYNCHRONOUS=
!*                               Specifier in I/O Statements
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : January 24, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS= Specifier in Data Transfer
!*                               Statements
!*  SECONDARY FUNCTIONS TESTED : scalar-char-initialization-expr is neither
!*                               'YES' nor 'NO'
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ(), WRITE(), ASYNCHRONOUS= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                :
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

PROGRAM asynchSpecDataXfer02d
    IMPLICIT NONE

    INTEGER :: i = 9
    INTEGER :: ioStatus

    CHARACTER(len = 256) :: ioErrorMsg


    OPEN(8, ACTION='write', ASYNCHRONOUS='YES',&
                &IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "OPEN(ACTION='WRITE'): ioStatus = (",&
                                &ioStatus, ") ", ioErrorMsg
        CALL zzrc( 1 )
    END IF


    WRITE(8, *, ASYNCHRONOUS='MAYBE', IOSTAT=ioStatus, IOMSG=ioErrorMsg) i
    IF (ioStatus /= 0) THEN
        PRINT *, "WRITE(ASYNCHRONOUS='MAYBE'): ioStatus = (",&
                                    &ioStatus, ") ", ioErrorMsg
        CALL zzrc( 2 )
    END IF


    CLOSE(8, IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "CLOSE(): ioStatus = (", ioStatus, ") ", ioErrorMsg
        CALL zzrc( 3 )
    END IF


    OPEN(8, ACTION='read', ASYNCHRONOUS='YES',&
                &IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "OPEN(ACTION='READ'): ioStatus = (",&
                            &ioStatus, ") ", ioErrorMsg
        CALL zzrc( 4 )
    END IF


    READ(8, *, ASYNCHRONOUS='MAYBE', IOSTAT=ioStatus, IOMSG=ioErrorMsg) i
    IF (ioStatus /= 0) THEN
        PRINT *, "READ(ASYNCHRONOUS='MAYBE'): ioStatus = (",&
                                    &ioStatus, ") ", ioErrorMsg
        CALL zzrc( 5 )

    ELSE IF (i /= 9) THEN
        PRINT *, "READ(ASYNCHRONOUS='YES'): (i != 9), i = (", i, ")"
        CALL zzrc( 6 )
    END IF


    CLOSE(8, IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "CLOSE(): ioStatus = (", ioStatus, ") ", ioErrorMsg
        CALL zzrc( 7 )
    END IF


END PROGRAM asynchSpecDataXfer02d
