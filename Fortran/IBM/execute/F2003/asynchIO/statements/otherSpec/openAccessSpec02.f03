!*  ===================================================================
!*
!*                               Specifier in I/O Statements
!*
!*  DATE                       : February 20, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS= Specifier in OPEN() Statement
!*  SECONDARY FUNCTIONS TESTED : ACCESS=Direct Specifier in OPEN() Statement
!*
!*  REQUIRED COMPILER OPTIONS  : -qintsize=8
!*
!*  KEYWORD(S)                 : OPEN(), ASYNCHRONOUS= Specifier,
!*                               ACCESS= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  9.4.5 The OPEN statement
!*  R904 open-stmt     is  OPEN ( connect-spec-list )
!*  R905 connect-spec  is  [ UNIT = ] file-unit-number
!*                     or  ACCESS = scalar-default-char-expr
!*                     or  ACTION = scalar-default-char-expr
!*                     or  ASYNCHRONOUS = scalar-default-char-expr
!*  ...
!*
!*  9.4.5.1 ACCESS= specifier in the OPEN statement
!*
!*  The scalar-default-char-expr shall evaluate to SEQUENTIAL, DIRECT, or
!*  STREAM.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mIOStatus
    IMPLICIT NONE

    TYPE tIOStatus
        INTEGER :: ioUnit
        INTEGER :: ioStatus
        CHARACTER(LEN = 256) :: ioMessage
    END TYPE tIOStatus
END MODULE mIOStatus


MODULE mFileStatus
    USE mIOStatus

    TYPE, EXTENDS(tIOStatus) :: tFileStatus
        CHARACTER(LEN = 30) :: fileName
        CHARACTER(LEN = 30) :: fileAccess
        CHARACTER(LEN = 30) :: asynchIOType
        INTEGER :: recordLength
    END TYPE tFileStatus
END MODULE mFileStatus


PROGRAM openAccessSpec02
    USE mFileStatus

    IMPLICIT NONE

    INTEGER :: aioID
    INTEGER :: recordID = 1

    TYPE(tFileStatus) :: fileStatus =&
            &tFileStatus(28,0,'',&
                        &'openAccessSpec02.dat','direct','yes', 30)


    OPEN(UNIT=fileStatus%ioUnit,&
        &FILE=fileStatus%fileName,&
        &ACCESS=fileStatus%fileAccess,&
        &ASYNCHRONOUS=fileStatus%asynchIOType,&
        &RECL=fileStatus%recordLength,&
        &IOSTAT=fileStatus%ioStatus,&
        &IOMSG=fileStatus%ioMessage)

    IF (fileStatus%ioStatus /= 0) THEN
        WRITE(0, *) "OPEN() <", fileStatus%ioStatus, "> ", fileStatus%ioMessage
        CALL zzrc( 1 )
    END IF


    WRITE(UNIT=fileStatus%ioUnit,&
        &IOSTAT=fileStatus%ioStatus,&
        &ASYNCHRONOUS='yes', REC=recordID, ID=aioID,&
        &IOMSG=fileStatus%ioMessage) fileStatus%asynchIOType

    IF (fileStatus%ioStatus /= 0) THEN
        WRITE(0, *) "WRITE(Asynchronous) <",&
            &fileStatus%ioStatus, "> ", fileStatus%ioMessage
        CALL zzrc( 2 )
    END IF


    recordID = recordID + 1
    WRITE(UNIT=fileStatus%ioUnit,&
        &REC=recordID,&
        &IOSTAT=fileStatus%ioStatus,&
        &IOMSG=fileStatus%ioMessage) fileStatus%fileName

    IF (fileStatus%ioStatus /= 0) THEN
        WRITE(0, *) "WRITE() <",&
            &fileStatus%ioStatus, "> ", fileStatus%ioMessage
        CALL zzrc( 3 )
    END IF


    recordID = recordID + 1
    WRITE(UNIT=fileStatus%ioUnit,&
        &IOSTAT=fileStatus%ioStatus,&
        &ASYNCHRONOUS='yes', REC=recordID,&
        &IOMSG=fileStatus%ioMessage) fileStatus%fileAccess

    IF (fileStatus%ioStatus /= 0) THEN
        WRITE(0, *) "WRITE(Asynchronous) <",&
            &fileStatus%ioStatus, "> ", fileStatus%ioMessage
        CALL zzrc( 4 )
    END IF


    WAIT(UNIT=fileStatus%ioUnit,&
        &ID=aioID,&
        &IOSTAT=fileStatus%ioStatus,&
        &IOMSG=fileStatus%ioMessage)

    IF (fileStatus%ioStatus /= 0) THEN
        WRITE(0, *) "WAIT(ID=", aioID, ") <",&
            &fileStatus%ioStatus, "> ", fileStatus%ioMessage
        CALL zzrc( 5 )
    END IF


    CLOSE(UNIT=fileStatus%ioUnit,&
        &IOSTAT=fileStatus%ioStatus,&
        &IOMSG=fileStatus%ioMessage)

    IF (fileStatus%ioStatus /= 0) THEN
        WRITE(0, *) "CLOSE() <",&
            &fileStatus%ioStatus, "> ", fileStatus%ioMessage
        CALL zzrc( 6 )
    END IF

END PROGRAM openAccessSpec02