!*  ===================================================================
!*
!*  DATE                       : March 28, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Error Condition in the 2nd of Three
!*                               Pending Unformatted Data Transfers
!*  SECONDARY FUNCTIONS TESTED : IOSTAT= and IOMSG= Specifiers of a subsequent
!*                               Synchronous Data Transfer should correctly
!*                               report this Condition
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ(), ASYNCHRONOUS=, ID=, IOSTAT=, and
!*                               IOMSG= Specifiers
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  9.5 Data transfer statements
!*
!*  R910 read-stmt        is  READ ( io-control-spec-list )[ input-item-list ]
!*  R911 write-stmt       is  WRITE ( io-control-spec-list )[ output-item-list ]
!*
!*  9.5.1 Control information list
!*
!*  R913 io-control-spec  is  [ UNIT = ] io-unit
!*                        or  [ FMT = ] format
!*                        or  [ NML = ] namelist-group-name
!*                        or  ADVANCE = scalar-default-char-expr
!*                        or  ASYNCHRONOUS = scalar-char-initialization-expr
!*  ...
!*                        or  END = label
!*                        or  EOR = label
!*                        or  ERR = label
!*                        or  ID = scalar-int-variable
!*                        or  IOMSG = iomsg-variable
!*                        or  IOSTAT = scalar-int-variable
!*  ...
!*                        or  SIZE = scalar-int-variable
!*
!*  9.5.3 Execution of a data transfer input/output statement
!*
!*  The effect of executing a synchronous data transfer input/output
!*  statement shall be as if the following operations were performed in the
!*  order specified:
!*
!*  ...
!*
!*  (3) Perform a wait operation for all pending input/output operations for
!*      the unit. If an error, end-of-file, or end-of-record condition occurs
!*      during any of the wait operations, steps 4 through 8 are skipped for
!*      the current data transfer statement.
!*
!*  9.6.2 Wait operation
!*
!*  ...
!*
!*  A wait operation completes the processing of a pending data transfer
!*  operation.
!*
!*  NOTE 9.53
!*
!*  Error, end-of-file, and end-of-record conditions may be raised either
!*  during the data transfer statement that initiates asynchronous
!*  input/output, a subsequent asynchronous data transfer statement for the
!*  same unit, or during the wait operation. If such conditions are raised
!*  during a data transfer statement, they trigger actions according to the
!*  IOSTAT=, ERR=, END=, and EOR= specifiers of that statement; ...
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM pendingSynchRead02

    INTEGER, DIMENSION( 3 ) :: ioID
    INTEGER, DIMENSION( 4 ) :: dataValue

    INTEGER, DIMENSION( 4 ) :: recNum = (/ 1, 22, 3, 4/)

    CHARACTER(LEN = 256) :: iMsg


    OPEN(21, action='read', access='direct',&
        recl=4, asynchronous='yes', form='unformatted')


    DO i = 1, 3
        READ(21, asynchronous='yes',&
            rec=recNum( i ), id=ioID( i )) dataValue( i )
    END DO


    READ(21, rec=4, iostat=iStat, iomsg=iMsg) dataValue( 4 )
    WRITE(6, *) "READ() <", iStat, "> ", iMsg

    IF (1 <> iStat) THEN
        CALL zzrc( 11 )
    END IF


    DO i = 3, 1, -1
        WAIT(21, id=ioID( i ), iostat=iStat, iomsg=iMsg)
        IF (224 <> iStat) THEN
            WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
            CALL zzrc( i + 20 )
        END IF
    END DO


    CLOSE( 21 )

END PROGRAM pendingSynchRead02
