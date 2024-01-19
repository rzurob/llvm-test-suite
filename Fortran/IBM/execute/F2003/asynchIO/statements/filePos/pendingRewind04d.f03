!*  ===================================================================
!*
!*  DATE                       : March 29, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Unformatted READ() Pending Data Transfers
!*                               with an end-of-file Condition
!*  SECONDARY FUNCTIONS TESTED : REWIND Statement; neither the ERR= nor
!*                               IOSTAT= Specifiers are *NOT* present
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ(), REWIND, ERR= Specifier, IOSTAT=
!*                               Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  9.6.2 Wait operation
!*
!*  If an error or end-of-file condition occurs during a wait operation for
!*  a unit, the processor performs a wait operation for all pending data
!*  transfer operations for that unit.
!*
!*  NOTE 9.53
!*  Error, end-of-file, and end-of-record conditions may be raised either
!*  during the data transfer statement that initiates asynchronous
!*  input/output, a subsequent asynchronous data transfer statement for the
!*  same unit, or during the wait operation. If such conditions are raised
!*  during a data transfer statement, they trigger actions according to the
!*  IOSTAT=, ERR=, END=, and EOR= specifiers of that statement; if they are
!*  raised during the wait operation, the actions are in accordance with the
!*  specifiers of the statement that performs the wait operation.
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

program pendingRewind04d

    integer, dimension( 4 ) :: ioID
    integer, dimension( 4 ) :: dataValue

    character(len = 256) :: iMsg


    open(41, asynchronous='yes', form='unformatted',&
        action='readwrite', access='stream', ioMsg=iMsg, iostat=iStat)
    if (iStat <> 0) then
        write(0, *) "OPEN() <", iStat, "> ", iMsg
        call zzrc( 11 )
    end if


    do i = 1, 3
        write(41, asynchronous='yes', id=ioID( i ),&
                &ioMsg=iMsg, iostat=iStat) ((i + 13) * 291)

        if (iStat <> 0) then
            write(0, *) i, "WRITE() <", iStat, "> ", iMsg
            call zzrc( (20 + i) )
        end if
    end do


    rewind(41, iostat=iStat, ioMsg=iMsg)
    if (iStat <> 0) then
        write(0, *) "REWIND() <", iStat, "> ", iMsg
        call zzrc( 31 )
    end if


    do i = 1, 4
        read(41, asynchronous='yes', id=ioID( i ),&
                &ioMsg=iMsg, iostat=iStat) dataValue( i )

        if (iStat <> 0) then
            write(0, *) i, "READ() <", iStat, "> ", iMsg
            call zzrc( (40 + i) )
        end if
    end do


    rewind( 41 )


    close(41, iostat=iStat, ioMsg=iMsg)
    if (iStat <> 0) then
        write(0, *) "CLOSE() <", iStat, "> ", iMsg
        call zzrc( 51 )
    end if

end program pendingRewind04d
