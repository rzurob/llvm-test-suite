!*  ===================================================================
!*
!*  DATE                       : March 24, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Pending Asynchronous Data Transfers
!*  SECONDARY FUNCTIONS TESTED : CLOSE() Performs a Wait Operation
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : CLOSE(), WAIT(), ID= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  9.4.6 The CLOSE statement
!*
!*  The CLOSE statement is used to terminate the connection of a specified
!*  unit to an external file.
!*
!*  Execution of a CLOSE statement performs a wait operation for any pending
!*  asynchronous data transfer operations for the specified unit.
!*
!*  ...
!*
!*  R908 close-stmt  is  CLOSE ( close-spec-list )
!*  R909 close-spec  is  [ UNIT = ] file-unit-number
!*                   or  IOSTAT = scalar-int-variable
!*                   or  IOMSG = iomsg-variable
!*                   or  ERR = label
!*                   or  STATUS = scalar-default-char-expr
!*
!*  9.6.1 WAIT statement
!*
!*  A WAIT statement performs a wait operation for specified pending
!*  asynchronous data transfer operations.
!*
!*  NOTE 9.51
!*  The CLOSE, INQUIRE, and file positioning statements may also perform wait
!*  operations.
!*
!*  R921 wait-stmt   is  WAIT (wait-spec-list)
!*  R922 wait-spec   is  [ UNIT = ] file-unit-number
!*                   or  END = label
!*                   or  EOR = label
!*                   or  ERR = label
!*                   or  ID = scalar-int-expr
!*  ...
!*
!*  9.6.2 Wait operation
!*
!*  A wait operation completes the processing of a pending data transfer
!*  operation. Each wait operation completes only a single data transfer
!*  operation, although a single statement may perform multiple wait
!*  operations.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM pendingClose01

    INTEGER, DIMENSION( 1000 ) :: ioID

    CHARACTER(LEN = 256) :: iMsg


    OPEN(185, ASYNCHRONOUS='yes', ACCESS='stream', IOMSG=iMsg,&
                &ACTION='write', FORM='unformatted', IOSTAT=iStat)
    IF (0 <> iStat) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    DO i = 1, 1000
        WRITE(185, ASYNCHRONOUS='yes', ID=ioID( i ),&
                &IOSTAT=iStat, IOMSG=iMsg) ((i * 329) - 456)
        IF (0 <> iStat) THEN
            WRITE(0, *) i, "WRITE() <", iStat, "> ", iMsg
            CALL zzrc( 2 )
        END IF
    END DO


    CLOSE(185, IOMSG=iMsg, IOSTAT=iStat)
    IF (0 <> iStat) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 3 )
    END IF


    DO i = 1000, 1, -1
        WAIT(185, ID=ioID( i ), IOSTAT=iStat, IOMSG=iMsg)
        IF (224 <> iStat) THEN
            WRITE(0, *) i, "WAIT(ID=", ioID( i ), ") <", iStat, "> ", iMsg
            CALL zzrc( 4 )
        END IF
    END DO

END PROGRAM pendingClose01
