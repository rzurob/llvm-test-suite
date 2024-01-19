!*  ===================================================================
!*
!*  DATE                       : March 22, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : INQUIRE() with both the PENDING= Specifier
!*                               and ID= Specifiers for specific Pending
!*                               Data Transfers
!*  SECONDARY FUNCTIONS TESTED : INQUIRE() with only the PENDING= Specifier
!*                               followed by an INQUIRE() on all of the
!*                               Pending Data Transfers
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WRITE(), INQUIRE(), PENDING= Specifier,
!*                               ID= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  To confirm that an INQUIRE() without the ID= Specifier performs a Wait
!*  Operation on all remaining Pending Data Transfers.
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

program pendingNoIDSpec03

    integer, dimension( 10 ) :: ioID

    logical :: iPENDING

    character(len = 256) :: iMsg


    OPEN(2012, ASYNCHRONOUS='yes', FORM='unformatted',&
        ACTION='write', ACCESS='stream', IOSTAT=iStat, IOMSG=iMsg)
    IF (0 /= iStat) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    do i = 1, 10
        write(2012, asynchronous='yes', id=ioID( i ),&
                iostat=iStat, iomsg=iMsg) ((i * 7) + i)

        if (0 /= iStat) then
            write(0, *) "WRITE() <", iStat, "> ", iMsg
            call zzrc( i + 10 )
        end if
    end do


    CALL InquireOnIDs(2012, ioID, 2, 0, 20)


    inquire(2012, pending=iPENDING, iostat=iStat, iomsg=iMsg)
    if (0 /= iStat) then
        write(0, *) "INQUIRE() <", iStat, "> ", iMsg
        call zzrc( 41 )
    end if


    CALL InquireOnIDs(2012, ioID, 1, 226, 60)


    CLOSE(2012, IOSTAT=iStat, IOMSG=iMsg)
    IF (0 /= iStat) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 81 )
    END IF

end program pendingNoIDSpec03


subroutine InquireOnIDs(ioUnit, ids, interval, expectedIStat, failRCBase)

    integer :: ioUnit
    integer, dimension( 10 ) :: ids
    integer :: interval
    integer :: expectedIStat
    integer :: failRCBase

    logical :: iPENDING
    character(len = 256) :: iMsg = ''


    DO i = 1, 10, interval
        INQUIRE(ioUnit, ID=ids( i ),&
                PENDING=iPENDING, IOSTAT=iStat, IOMSG=iMsg)

        IF (expectedIStat /= iStat) THEN
            WRITE(0, *) "INQUIRE(ID=", ids( i ), ") <", iStat, "> ", iMsg
            CALL zzrc( i + failRCBase )
        END IF
    END DO

end subroutine InquireOnIDs
