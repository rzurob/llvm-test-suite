!*  ===================================================================
!*
!*  DATE                       : March 31, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Error Condition in one of Three Pending
!*                               Unformatted Data Transfers
!*  SECONDARY FUNCTIONS TESTED : IOSTAT=, IOMSG=, and ERR= Specifiers of a
!*                               subsequent Synchronous Data Transfer are
!*                               *NOT* present to report this Condition;
!*                               Synchronous Data Transfer should terminate
!*                               Test Case
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ()
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

PROGRAM pendingSynchRead04d

    integer, dimension( 5 ) :: recNum
    integer, dimension( 5 ) :: dataValue

    character(len = 256) :: iMsg


    open(962, action='readwrite', access='direct', iomsg=iMsg,&
        recl=4, asynchronous='yes', form='unformatted', iostat=iStat)
    if (0 <> iStat) then
        write(0, *) "OPEN() <", iStat, "> ", iMsg
        call zzrc( 11 )
    end if


    do i = 5, 1, -1
        recNum( i ) = i + 1

        write(962, rec=i, iostat=iStat, ioMsg=iMsg) recNum( i )

        if (0 <> iStat) then
            write(0, *) "WRITE(REC=", i, ") <", iStat, "> ", iMsg
            call zzrc( (20 + i) )
        end if
    end do


    write(0, '(5I2)') (recNum( i ), i = 1, 5)


    do i = 1, 5
        read(962, asynchronous='yes', ioMsg=iMsg,&
            rec=recNum( i ), iostat=iStat) dataValue( i )

        if (0 <> iStat) then
            write(0, *) i, "READ(REC=", recNum( i ), ") <", iStat, "> ", iMsg
            call zzrc( (30 + i) )
        end if
    end do


    read(962, rec=1) dataValue( 5 )

    write(0, '(5I2)') (dataValue( i ), i = 1, 5)


    close(962, iomsg=iMsg, iostat=iStat)
    if (0 <> iStat) then
        write(0, *) "CLOSE() <", iStat, "> ", iMsg
        call zzrc( 41 )
    end if

END PROGRAM pendingSynchRead04d
