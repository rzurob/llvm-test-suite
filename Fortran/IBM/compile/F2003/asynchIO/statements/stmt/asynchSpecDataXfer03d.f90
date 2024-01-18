!*  ===================================================================
!*
!*                               Specifier in I/O Statements
!*
!*  DATE                       : January 24, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS= Specifier in Data Transfer
!*                               Statements
!*  SECONDARY FUNCTIONS TESTED : io-unit is either "*" or an Internal File
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ(), WRITE(), ASYNCHRONOUS= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 4
!*
!*  DESCRIPTION                :
!*
!*  9.4 File connection
!*
!*  R901 io-unit                 is  file-unit-number
!*                               or  *
!*                               or  internal-file-variable
!*  R902 file-unit-number        is  scalar-int-expr
!*  R903 internal-file-variable  is  char-variable
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
!*  C925 (R913) An ASYNCHRONOUS= specifier with a value YES shall not appear
!*              unless io-unit is a file-unit-number.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM asynchSpecDataXfer03d
    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: ioStatus

    CHARACTER(len = 256) :: readBuffer = '15'
    CHARACTER(len = 256) :: writeBuffer
    CHARACTER(len = 256) :: ioErrorMsg


    !
    !  Write Asynchronously to an Internal File.
    !
    WRITE(writeBuffer, *, ASYNCHRONOUS='YES   ',&
            &IOSTAT=ioStatus, IOMSG=ioErrorMsg) i
    IF (ioStatus /= 0) THEN
        PRINT *, "WRITE(internalFile,ASYNCHRONOUS='yes'): ioStatus = (",&
                                                &ioStatus, ") ", ioErrorMsg
        CALL zzrc( 1 )
    END IF


    !
    !  Write Asynchronously to STDOUT.
    !
    WRITE(*, *, ASYNCHRONOUS='yes',&
            &IOSTAT=ioStatus, IOMSG=ioErrorMsg) i
    IF (ioStatus /= 0) THEN
        PRINT *, "WRITE(*,ASYNCHRONOUS='yes'): ioStatus = (",&
                                    &ioStatus, ") ", ioErrorMsg
        CALL zzrc( 2 )
    END IF


    !
    !  Write Asynchronously from an Internal File.
    !
    READ(readBuffer, *, ASYNCHRONOUS='yes',&
            &IOSTAT=ioStatus, IOMSG=ioErrorMsg) i
    IF (ioStatus /= 0) THEN
        PRINT *, "READ(internalFile,ASYNCHRONOUS='yes'): ioStatus = (",&
                                                &ioStatus, ") ", ioErrorMsg
        CALL zzrc( 3 )
    END IF


    !
    !  Read Asynchronously from STDIN.
    !
    READ(*, *, ASYNCHRONOUS='YES  ',&
            &IOSTAT=ioStatus, IOMSG=ioErrorMsg) i
    IF (ioStatus /= 0) THEN
        PRINT *, "READ(*,ASYNCHRONOUS='yes'): ioStatus = (",&
                                    &ioStatus, ") ", ioErrorMsg
        CALL zzrc( 4 )
    END IF


END PROGRAM asynchSpecDataXfer03d
