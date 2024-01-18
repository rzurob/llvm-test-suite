!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : pendingClose02 - CLOSE() Statement
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : March 24, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pending Asynchronous Data Transfers with
!*                               an Error
!*  SECONDARY FUNCTIONS TESTED : CLOSE() Performs a Wait Operation
!*
!*  DRIVER STANZA              : xlf2003
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
!*
!*  9.6.2 Wait operation
!*
!*  A wait operation completes the processing of a pending data transfer
!*  operation. Each wait operation completes only a single data transfer
!*  operation, although a single statement may perform multiple wait
!*  operations.
!*
!*  If an error or end-of-file condition occurs during a wait operation
!*  for a unit, the processor performs a wait operation for all pending
!*  data transfer operations for that unit.
!*
!*  NOTE 9.53
!*  Error, end-of-file, and end-of-record conditions may be raised either
!*  during the data transfer statement that initiates asynchronous
!*  input/output, a subsequent asynchronous data transfer statement for
!*  the same unit, or during the wait operation. ...  if they are raised
!*  during the wait operation, the actions are in accordance with the
!*  specifiers of the statement that performs the wait operation.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM pendingClose02

    INTEGER, DIMENSION( 10 ) :: ioID
    INTEGER, DIMENSION( 10 ) :: dataValue = 0

    INTEGER, DIMENSION( 10 ) :: recNum = (/ (i, i = 1, 10) /)

    CHARACTER(LEN = 256) :: iMsg


    recNum( 5 ) = 55

    OPEN(207, ACTION='read', ACCESS='direct', RECL=4,&
        FILE='pendingClose02.dat', ASYNCHRONOUS='yes',&
            FORM='unformatted', IOMSG=iMsg, IOSTAT=iStat)

    IF (0 <> iStat) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    DO i = 1, 10
        READ(207, ASYNCHRONOUS='yes', ID=ioID( i ),&
            REC=recNum( i ), IOSTAT=iStat, IOMSG=iMsg) dataValue( i )

        IF (0 <> iStat) THEN
            WRITE(0, *) "READ() <", iStat, "> ", iMsg
            CALL zzrc( 10 + i )
        END IF
    END DO


    CLOSE(207, IOSTAT=iStat, IOMSG=iMsg)
    IF (1 <> iStat) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 31 )
    END IF


    DO i = 1, 10
        WAIT(207, ID=ioID( i ), IOSTAT=iStat, IOMSG=iMsg)

        IF (224 <> iStat) THEN
            WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
            CALL zzrc( 40 + i )
        END IF
    END DO


    PRINT '(10I6)', (dataValue( i ), i = 1, 10)

END PROGRAM pendingClose02
