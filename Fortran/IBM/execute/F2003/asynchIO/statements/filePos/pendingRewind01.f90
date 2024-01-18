!*  ===================================================================
!*
!*  DATE                       : March 17, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Perform Unformatted Asynchronous WRITE()
!*                               Operations
!*  SECONDARY FUNCTIONS TESTED : Perform a REWIND, and READ() the data
!*                               written
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : REWIND, WRITE(), READ(), ID= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  9.7 File positioning statements
!*
!*  R925 rewind-stmt     is  REWIND file-unit-number
!*                       or  REWIND ( position-spec-list )
!*
!*  R926 position-spec   is  [ UNIT = ] file-unit-number
!*                       or  IOMSG = iomsg-variable
!*                       or  IOSTAT = scalar-int-variable
!*                       or  ERR = label
!*
!*  Execution of a file positioning statement performs a wait operation
!*  for all pending asynchronous data transfer operations for the
!*  specified unit.
!*
!*  9.7.3 REWIND statement
!*
!*  Execution of a REWIND statement causes the specified file to be
!*  positioned at its initial point.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM pendingRewind01

    INTEGER, DIMENSION( 10 ) :: ioID

    INTEGER, DIMENSION( 10 ) :: inData
    INTEGER, DIMENSION( 10 ) :: outData = (/ (((i * 3167) + 6317), i = 1, 10) /)

    CHARACTER(LEN = 256) :: iMsg


    OPEN(6317, ASYNCHRONOUS='yes', ACTION='readwrite',&
        &ACCESS='stream', FORM='unformatted', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    DO i = 1, 10
        WRITE(6317, ID=ioID( i ), ASYNCHRONOUS='yes',&
                &IOSTAT=iStat, IOMSG=iMsg) outData( i )
        IF (iStat <> 0) THEN
            WRITE(0, *) "WRITE() <", iStat, "> ", iMsg
            CALL zzrc( (10 + i) )
        END IF
    END DO


    REWIND(6317, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "REWIND() <", iStat, "> ", iMsg
        CALL zzrc( 21 )
    END IF


    DO i = 10, 1, -1
        WAIT(6317, ID=ioID( i ), IOSTAT=iStat, IOMSG=iMsg)
        IF (iStat <> 224) THEN
            WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
            CALL zzrc( (30 + i) )
        END IF
    END DO


    WRITE(6, '(10I6)') (outData( i ), i = 1, 10)


    DO i = 1, 10
        READ(6317, ID=ioID( i ), ASYNCHRONOUS='yes',&
                &IOSTAT=iStat, IOMSG=iMsg) inData( i )
        IF (iStat <> 0) THEN
            WRITE(0, *) "READ() <", iStat, "> ", iMsg
            CALL zzrc( (40 + i) )
        END IF
    END DO


    CLOSE(6317, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 51 )
    END IF


    WRITE(6, '(10I6)') (inData( i ), i = 1, 10)


    DO i = 1, 10
        IF (outData( i ) <> inData( i )) THEN
            WRITE(0, *) " inData(", i, ") = '", inData( i ), "'"
            WRITE(0, *) "outData(", i, ") = '", outData( i ), "'"
            CALL zzrc( (60 + i) )
        END IF
    END DO

END PROGRAM pendingRewind01
