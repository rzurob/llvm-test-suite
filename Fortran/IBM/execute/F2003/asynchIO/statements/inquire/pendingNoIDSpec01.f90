!*  ===================================================================
!*
!*  DATE                       : March 21, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Perform Unformatted Asynchronous I/O
!*                               Operations on a Unit
!*  SECONDARY FUNCTIONS TESTED : Perform an INQUIRE() with the PENDING=
!*                               Specifier (ID= Specifier is *NOT* present)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ(), WRITE(), INQUIRE(), PENDING=
!*                               Specifier, ID= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  9.9 File inquiry
!*
!*  R929 inquire-stmt  is  INQUIRE ( inquire-spec-list )
!*
!*  9.9.1 Inquiry specifiers
!*
!*  R930 inquire-spec  is  [ UNIT = ] file-unit-number
!*                     or  FILE = file-name-expr
!*                     or  ACCESS = scalar-default-char-variable
!*                     or  ACTION = scalar-default-char-variable
!*                     or  ASYNCHRONOUS = scalar-default-char-variable
!*  ...
!*                     or  ID = scalar-int-expr
!*  ...
!*                     or  PENDING = scalar-default-logical-variable
!*  ...
!*
!*  C950 (R930) If an ID= specifier appears, a PENDING= specifier shall
!*              also appear.
!*
!*  9.9.1.13 ID= specifier in the INQUIRE statement
!*
!*  The value of the expression specified in the ID= specifier shall be the
!*  identifier of a pending data transfer operation for the specified unit.
!*
!*  9.9.1.20 PENDING= specifier in the INQUIRE statement
!*
!*  The PENDING= specifier is used to determine whether or not previously
!*  pending asynchronous data transfers are complete. A data transfer
!*  operation is previously pending if it is pending at the beginning of
!*  execution of the INQUIRE statement.
!*
!*  If the ID= specifier is omitted and all previously pending data
!*  transfer operations for the specified unit are complete, then the
!*  variable specified in the PENDING= specifier is assigned the value
!*  false and the INQUIRE statement performs wait operations for all
!*  previously pending data transfers for the specified unit.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program pendingNoIDSpec01

    INTEGER, DIMENSION( 10 ) :: ioID
    INTEGER, DIMENSION( 10 ) :: dataIn
    INTEGER, DIMENSION( 10 ) :: dataOut = (/ ((i * 5123), i = 1, 10) /)

    CHARACTER(LEN = 256) :: iMsg


    ioUnit = 32
    open(ioUnit, ASYNCHRONOUS='yes', ACCESS='sequential',&
        &ACTION='readwrite', FORM='unformatted', IOSTAT=iStat, IOMSG=iMsg)
    if (iStat /= 0) then
        write(0, *) "OPEN() <", iStat, "> ", iMsg
        call zzrc( 1 )
    end if


    do i = 1, 10
        write(ioUnit, ASYNCHRONOUS='yes', ID=ioID( i ),&
                &IOSTAT=iStat, IOMSG=iMsg) dataOut( i )

        if (iStat /= 0) then
            write(0, *) i, ") WRITE() <", iStat, "> ", iMsg
            call zzrc( (10 + i) )
        end if
    end do


    call PerformInquire(ioUnit, ioID, 20)


    rewind ioUnit


    do i = 1, 10
        read(ioUnit, ASYNCHRONOUS='yes', ID=ioID( i ),&
                &IOSTAT=iStat, IOMSG=iMsg) dataIn( i )

        if (iStat /= 0) then
            write(0, *) i, ") READ() <", iStat, "> ", iMsg
            call zzrc( (60 + i) )
        end if
    end do


    call PerformInquire(ioUnit, ioID, 70)


    do i = 1, 10
        if (dataOut( i ) /= dataIn( i )) then
            write(0, *) " dataIn(", i, ") = '", dataIn( i ), "'"
            write(0, *) "dataOut(", i, ") = '", dataOut( i ), "'"

            call zzrc( 111 )
        end if
    end do


    close(ioUnit, IOSTAT=iStat, IOMSG=iMsg)
    if (iStat /= 0) then
        write(0, *) "CLOSE() <", iStat, "> ", iMsg
        call zzrc( 121 )
    end if

end program pendingNoIDSpec01


subroutine PerformInquire(theUnit, idList, failRCbase)

    INTEGER, INTENT(IN) :: theUnit
    INTEGER, DIMENSION( 10 ), INTENT(IN) :: idList
    INTEGER, INTENT(IN) :: failRCbase

    LOGICAL :: apPending
    LOGICAL :: prePending

    CHARACTER(LEN = 256) :: iMsg = ''


    inquire(theUnit, PENDING=prePending, IOSTAT=iStat, IOMSG=iMsg)
    if (iStat /= 0) then
        write(0, *) "INQUIRE() <", iStat, "> ", iMsg
        call zzrc( (failRCbase + 1) )
    end if


    do i = 1, 10
        inquire(theUnit, ID=idList( i ),&
                &PENDING=apPending, IOSTAT=iStat, IOMSG=iMsg)

        if (( prePending )  .AND.  (iStat /= 0)) then
            write(0, *) i, ") INQUIRE() <", iStat, "> ", iMsg
            call zzrc( (failRCbase + 20 + i) )

        else if (.NOT. prePending) then
            if (iStat /= 226) then
                write(0, *) i, ") INQUIRE() <", iStat, "> ", iMsg
                call zzrc( (failRCbase + 30 + i) )

            else if ( apPending ) then
                write(0, *) i, ") INQUIRE(PENDING=", apPending, ")"
                call zzrc( (failRCbase + 40 + i) )
            end if
        end if
    end do

end subroutine PerformInquire
