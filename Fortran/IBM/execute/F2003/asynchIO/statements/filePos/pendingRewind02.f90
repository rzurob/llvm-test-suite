!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : pendingRewind02 - REWIND Statement
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : March 28, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Unformatted READ() Pending Data Transfers
!*                               with an end-of-file Condition
!*  SECONDARY FUNCTIONS TESTED : REWIND Statement with the IOSTAT= and ERR=
!*                               Specifiers
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ(), REWIND, IOSTAT= Specifier, END=
!*                               Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
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
!*
!*  9.7.3 REWIND statement
!*
!*  Execution of a REWIND statement causes the specified file to be
!*  positioned at its initial point.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program pendingRewind02
    use ISO_FORTRAN_ENV

    integer, dimension( 3 ) :: ioID
    integer, dimension( 3 ) :: dataValue

    character(len = 256) :: iMsg = ''


    open(188, file='pendingRewind02.dat', action='read', access='stream',&
            form='unformatted', asynchronous='yes', iostat=iStat, iomsg=iMsg)
    if (iStat /= 0) then
        write(0, *) "OPEN() <", iStat, "> ", iMsg
        call zzrc( 1_4 )
    end if


    dataValue = 0
    do i = 1, 3
        read(188, asynchronous='yes',&
            id=ioID( i ), iostat=iStat, iomsg=iMsg) dataValue( i )

        if (iStat /= 0) then
            write(0, *) "READ() <", iStat, "> ", iMsg
            call zzrc( INT((i + 10), 4) )
        end if
    end do


    rewind(188, iostat=iStat, iomsg=iMsg)
    if (iStat /= IOSTAT_END) then
        write(0, *) "REWIND() <", iStat, "> ", iMsg
        call zzrc( 21_4 )
    end if


    call ConfirmWait(188, ioID, 30_4)

    write(6, '("IOSTAT= Specifier",3I6)') (dataValue( i ), i = 1, 3)


    dataValue = 0
    do i = 1, 3
        read(188, asynchronous='yes',&
            id=ioID( i ), iostat=iStat, iomsg=iMsg) dataValue( i )

        if (iStat /= 0) then
            write(0, *) "READ() <", iStat, "> ", iMsg
            call zzrc( INT((i + 40), 4) )
        end if
    end do


    rewind(188, ERR=100, iostat=iStat, iomsg=iMsg)

    write(6, *) "REWIND() <", iStat, "> ", iMsg
    call zzrc( 51_4 )

    goto 200

100 write(6, *) "REWIND(ERR=) <", iStat, "> ", iMsg
    if (iStat /= IOSTAT_END) then
        call zzrc( 61_4 )
    end if


200 call ConfirmWait(188, ioID, 70_4)

    write(6, '("   ERR= Specifier",3I6)') (dataValue( i ), i = 1, 3)


    close(188, iostat=iStat, iomsg=iMsg)
    if (iStat /= 0) then
        write(0, *) "CLOSE() <", iStat, "> ", iMsg
        call zzrc( 81_4 )
    end if

end program pendingRewind02


subroutine ConfirmWait(ioUnit, aIDs, failRCBase)

    integer, intent(in) :: ioUnit
    integer, dimension( 3 ), intent(in) :: aIDs
    integer(4), intent(in) :: failRCBase

    integer(4) :: i
    character(len = 256) :: iMsg = ''


    do i = 1_4, 3_4
        wait(ioUnit, id=aIDs( i ), iostat=iStat, iomsg=iMsg)

        if (iStat /= 224) then
            write(0, *) "WAIT() <", iStat, "> ", iMsg
            call zzrc( i + failRCbase )
        end if
    end do

end subroutine ConfirmWait
