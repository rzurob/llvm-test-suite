!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : pendingSynchRead03 - Wait Operation
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : March 31, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Error Condition in one of Three Pending
!*                               Unformatted Data Transfers
!*  SECONDARY FUNCTIONS TESTED : IOSTAT= and IOMSG= Specifiers of a subsequent
!*                               Synchronous Data Transfer should correctly
!*                               report this Condition; if present, ERR=
!*                               should also be handled correctly
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ(), ASYNCHRONOUS=, ID=, IOSTAT=, IOMSG=,
!*                               and ERR= Specifiers
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
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

PROGRAM pendingSynchRead03

    INTEGER, DIMENSION( 8 ) :: dataValue

    INTEGER, DIMENSION( 3 ) :: recNum = (/ 11, 2, 3 /)

    CHARACTER(LEN = 256) :: iMsg


    OPEN(940, asynchronous='YES', RECL=4,&
            file='pendingSynchRead03.dat',&
           access='DIRECT', action='READ', form='UNFORMATTED')


    DO i = 1, 3
        WRITE(0, *) i, ") READ(REC=", recNum( i ), ")"
        READ(940, asynchronous='YES', rec=recNum( i )) dataValue( i )
    END DO


    WRITE(0, *) "4 ) READ(REC= 1 )"

    READ(940, rec=1, err=100, iostat=iStat, iomsg=iMsg) dataValue( 4 )

    WRITE(0, *) "READ() <", iStat, "> ", iMsg
    CALL zzrc( 11 )


100 WRITE(0, *) "READ(ERR=100) <", iStat, "> ", iMsg

    IF (iStat <> 1) THEN
        WRITE(0, *) "READ() <", iStat, "> ", iMsg
        CALL zzrc( 12 )
    END IF


    DO i = 3, 1, -1
        WRITE(0, *) (i + 4), ") READ(REC=", recNum( i ), ")"
        READ(940, asynchronous='YES', rec=recNum( i )) dataValue( (4 + i) )
    END DO


    WRITE(0, *) "8 ) READ(REC= 1 )"

    READ(940, rec=1, err=200) dataValue( 8 )

    WRITE(0, *) "READ() should not be here"
    CALL zzrc( 13 )


200 WRITE(0, *) "READ(ERR=200)"


    CLOSE( 940 )

END PROGRAM pendingSynchRead03
