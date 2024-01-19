!*  ===================================================================
!*
!*  DATE                       : November 30, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Perform Two Unformatted Asynchronous
!*                               WRITE() Operations on a Unit OPEN()ed
!*                               for Write only
!*  SECONDARY FUNCTIONS TESTED : BACKSPACE Once after each WRITE() Operation
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

PROGRAM pendingBackspace02d

    INTEGER(4) :: i
    INTEGER, DIMENSION( 2 ) :: dataValues

    CHARACTER(LEN = 256) :: iMsg


    OPEN(2003, ASYNCHRONOUS='yes', ACCESS='sequential',&
        &ACTION='write', FORM='unformatted', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1_4 )
    END IF


    dataValues = [ (i, i = 1, 2) ]


    DO i = 1_4, 2_4
        WRITE(2003, ASYNCHRONOUS='yes', ID=ioID,&
              IOSTAT=iStat, IOMSG=iMsg) dataValues( i )
        IF (iStat /= 0) THEN
            WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
            CALL zzrc( ((i * 10_4) + 1_4) )
        END IF

        PRINT '(I2,I3)', i, dataValues( i )


        BACKSPACE(2003, IOSTAT=iStat, IOMSG=iMsg)
        IF (iStat /= 233) THEN
            CALL zzrc( ((i * 10_4) + 2_4) )
        END IF

        WRITE(0, *) "BACKSPACE() <", iStat, "> ", iMsg


        WAIT(2003, ID=ioID, IOSTAT=iStat, IOMSG=iMsg)
        IF (iStat /= 0) THEN
            WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
            CALL zzrc( ((i * 10_4) + 3_4) )
        END IF
    END DO


    CLOSE(2003, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 30_4 )
    END IF

END PROGRAM pendingBackspace02d
