!*  ===================================================================
!*
!*  DATE                       : March 17, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Perform Unformatted Asynchronous WRITE()
!*                               Operations
!*  SECONDARY FUNCTIONS TESTED : Perform a ENDFILE, and WRITE() further data
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ENDFILE, WRITE(), ID= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  9.7 File positioning statements
!*
!*  R924 endfile-stmt    is  ENDFILE file-unit-number
!*                       or  ENDFILE ( position-spec-list )
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
!*  9.7.2 ENDFILE statement
!*
!*  Execution of an ENDFILE statement for a file connected for sequential
!*  access writes an endfile record as the next record of the file. The
!*  file is then positioned after the endfile record, which becomes the
!*  last record of the file.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM pendingEndfile01

    INTEGER, DIMENSION( 16 ) :: aID
    INTEGER, DIMENSION( 10 ) :: dataValues = (/ (i, i = 100, 118, 2) /)

    CHARACTER(LEN = 256) :: iMsg


    OPEN(2038, ASYNCHRONOUS='yes', ACCESS='sequential',&
            ACTION='write', FILE='pendingEndfile01.dat',&
            FORM='unformatted', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        ERROR STOP 1
    END IF


    DO i = 10, 1 -1
        WRITE(2038, ASYNCHRONOUS='no',&
            IOSTAT=iStat, IOMSG=iMsg) dataValues( i )

        IF (iStat <> 0) THEN
            WRITE(0, *) "WRITE() <", iStat, "> ", iMsg
            CALL zzrc( (10 + i) )
        END IF
    END DO


    REWIND(2038, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "REWIND() <", iStat, "> ", iMsg
        ERROR STOP 21
    END IF


    DO i = 1, 5
        WRITE(2038, ASYNCHRONOUS='yes', ID=aID( i ),&
            IOSTAT=iStat, IOMSG=iMsg) dataValues( i )

        IF (iStat <> 0) THEN
            WRITE(0, *) "WRITE() <", iStat, "> ", iMsg
            CALL zzrc( (30 + i) )
        END IF
    END DO


    ENDFILE(2038, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "ENDFILE() <", iStat, "> ", iMsg
        ERROR STOP 41
    END IF


    DO i = 1, 5
        WAIT(2038, ID=aID( i ), IOSTAT=iStat, IOMSG=iMsg)
        IF (iStat <> 224) THEN
            WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
            CALL zzrc( (50 + i) )
        END IF
    END DO


    CLOSE(2038, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        ERROR STOP 61
    END IF

END PROGRAM pendingEndfile01
