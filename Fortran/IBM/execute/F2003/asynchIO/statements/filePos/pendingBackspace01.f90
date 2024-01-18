!*  ===================================================================
!*
!*  DATE                       : March 17, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Perform Two Unformatted Asynchronous
!*                               WRITE() Operations on a Unit OPEN()ed
!*                               for both Read and Write
!*  SECONDARY FUNCTIONS TESTED : BACKSPACE Once
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : BACKSPACE, WRITE(), ID= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  9.7 File positioning statements
!*
!*  R923 backspace-stmt  is  BACKSPACE file-unit-number
!*                       or  BACKSPACE ( position-spec-list )
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
!*  9.7.1 BACKSPACE statement
!*
!*  Execution of a BACKSPACE statement causes the file connected to the
!*  specified unit to be positioned before the current record if there
!*  is a current record, or before the preceding record if there is no
!*  current record. If the file is at its initial point, the position
!*  of the file is not changed.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM pendingBackspace01

    INTEGER, DIMENSION( 2 ) :: ioID
    INTEGER, DIMENSION( 2,10 ) :: dataValues

    CHARACTER(LEN = 256) :: iMsg


    OPEN(2003, ASYNCHRONOUS='yes', ACCESS='sequential',&
        &ACTION='readwrite', FORM='unformatted', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1_4 )
    END IF


    dataValues =&
        RESHAPE(&
            (/ ((((j * 1985) + (i * 2006)),&
                        j = 1, 2), i = 1, 10) /), (/ 2,10 /))


    DO i = 1, 10
        DO j = 1, 2
            WRITE(2003, ASYNCHRONOUS='yes', ID=ioID( j ),&
                IOSTAT=iStat, IOMSG=iMsg) dataValues( j,i )
            IF (iStat /= 0) THEN
                WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
                CALL zzrc( ((INT(i, 4) * 10_4) + INT(j, 4)) )
            END IF
        END DO

        PRINT '(I2,I6,I6)', i, dataValues( 1,i ), dataValues( 2,i )


        BACKSPACE(2003, IOSTAT=iStat, IOMSG=iMsg)
        IF (iStat /= 0) THEN
            WRITE(0, *) "BACKSPACE() <", iStat, "> ", iMsg
            CALL zzrc( ((INT(i, 4) * 10_4) + 3_4) )
        END IF


        DO j = 1, 2
            WAIT(2003, ID=ioID( j ), IOSTAT=iStat, IOMSG=iMsg)
            IF (iStat /= 224) THEN
                WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
                CALL zzrc( ((INT(i, 4) * 10_4) + 4_4) )
            END IF
        END DO
    END DO


    REWIND(2003, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "REWIND() <", iStat, "> ", iMsg
        CALL zzrc( 150_4 )
    END IF


    i = 0

    PRINT *
    DO WHILE (iStat == 0)
        i = i + 1

        READ(2003, END=100, IOSTAT=iStat, IOMSG=iMsg) j
        IF (iStat /= 0) THEN
            WRITE(0, *) "READ() <", iStat, "> ", iMsg
            CALL zzrc( ((INT(i, 4) * 10_4) + 5_4) )
        END IF

        PRINT '(I2,I6)', i, j
100 END DO


    CLOSE(2003, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 160_4 )
    END IF

END PROGRAM pendingBackspace01
