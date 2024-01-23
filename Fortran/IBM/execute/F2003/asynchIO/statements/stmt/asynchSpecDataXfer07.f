!*  ===================================================================
!*
!*                               Specifier in I/O Statements
!*
!*  DATE                       : February  1, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS='yes' Specifier in Data Transfer
!*                               Statement
!*  SECONDARY FUNCTIONS TESTED : File OPEN()ed with ASYNCHRONOUS='no'
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ(), WRITE(), ASYNCHRONOUS= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 3
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

PROGRAM asynchSpecDataXfer07d
    IMPLICIT NONE

    INTEGER :: i = 5
    INTEGER :: ioStatus

    CHARACTER(len = 256) :: ioErrorMsg


    OPEN(8, ACTION='write', ASYNCHRONOUS='YES',&
                &IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "OPEN(ACTION='WRITE'): ioStatus = (",&
                                &ioStatus, ") ", ioErrorMsg
        ERROR STOP 1
    END IF


    WRITE(8, *, ASYNCHRONOUS='YES', IOSTAT=ioStatus, IOMSG=ioErrorMsg) i
    IF (ioStatus /= 0) THEN
        PRINT *, "WRITE(ASYNCHRONOUS='YES'): ioStatus = (",&
                    &ioStatus, ") ", ioErrorMsg
        ERROR STOP 2
    END IF


    i = i + 1
    WRITE(8, *, ASYNCHRONOUS='NO', IOSTAT=ioStatus, IOMSG=ioErrorMsg) i
    IF (ioStatus /= 0) THEN
        PRINT *, "WRITE(ASYNCHRONOUS='NO'): ioStatus = (",&
                    &ioStatus, ") ", ioErrorMsg
        ERROR STOP 3
    END IF


    CLOSE(8, IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "CLOSE(): ioStatus = (", ioStatus, ") ", ioErrorMsg
        ERROR STOP 4
    END IF


    OPEN(8, ACTION='READ', ASYNCHRONOUS='YES',&
                &IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "OPEN(ACTION='READ'): ioStatus = (",&
                                &ioStatus, ") ", ioErrorMsg
        ERROR STOP 5
    END IF


    READ(8, *, ASYNCHRONOUS='NO', IOSTAT=ioStatus, IOMSG=ioErrorMsg) i
    IF (ioStatus /= 0) THEN
        PRINT *, "READ(ASYNCHRONOUS='NO'): ioStatus = (",&
                    &ioStatus, ") ", ioErrorMsg
        ERROR STOP 6

    ELSE IF (i /= 5) THEN
        PRINT *, "READ(ASYNCHRONOUS='NO'): (i != 5), i = (", i, ")"
        ERROR STOP 7
    END IF


    READ(8, *, ASYNCHRONOUS='YES', IOSTAT=ioStatus, IOMSG=ioErrorMsg) i
    IF (ioStatus /= 0) THEN
        PRINT *, "READ(ASYNCHRONOUS='YES'): ioStatus = (",&
                    &ioStatus, ") ", ioErrorMsg
        ERROR STOP 8

    ELSE IF (i /= 6) THEN
        PRINT *, "READ(ASYNCHRONOUS='YES'): (i != 6), i = (", i, ")"
        ERROR STOP 9
    END IF


    CLOSE(8, IOSTAT=ioStatus, IOMSG=ioErrorMsg)
    IF (ioStatus /= 0) THEN
        PRINT *, "CLOSE(): ioStatus = (", ioStatus, ") ", ioErrorMsg
        ERROR STOP 10
    END IF


END PROGRAM asynchSpecDataXfer07d
