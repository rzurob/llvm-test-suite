!*  ===================================================================
!*
!*  DATE                       : March 29, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Pending Data Transfers with an end-of-file
!*                               Condition
!*  SECONDARY FUNCTIONS TESTED : INQUIRE() with the ID=, PENDING=, and ERR=
!*                               Specifiers; the IOSTAT= Specifier is *NOT*
!*                               present
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : INQUIRE(), PENDING= Specifier, ID= Specifier
!*                               ERR= Specifier
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
!*                     or  ERR = label
!*  ...
!*                     or  ID = scalar-int-expr
!*  ...
!*                     or  IOSTAT = scalar-int-variable
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
!*  If an ID= specifier appears and the specified data transfer operation
!*  is complete, then the variable specified in the PENDING= specifier is
!*  assigned the value false and the INQUIRE statement performs the wait
!*  operation for the specified data transfer.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program pendingNoIOSTATSpec01d

    integer, dimension( 3 ) :: ioID
    integer, dimension( 3 ) :: dataValue

    character(len = 256) :: iMsg


    OPEN(43, action='read', access='stream',&
        &file='pendingNoIOSTATSpec01d.dat', iomsg=iMsg,&
        &asynchronous='yes', form='unformatted', iostat=iStat)
    IF (iStat /= 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 11_4 )
    END IF


    DO i = 1, 3
        READ(43, id=ioID( i ), asynchronous='yes',&
                &iostat=iStat, iomsg=iMsg) dataValue( i )

        IF (iStat /= 0) THEN
            WRITE(0, *) i, "READ() <", iStat, "> ", iMsg
            CALL zzrc( INT((20 + i), 4) )
        END IF
    END DO

    call InquireOnIDs(43, ioID)

    CLOSE(43, iostat=iStat, iomsg=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 51_4 )
    END IF

end program pendingNoIOSTATSpec01d


subroutine InquireOnIDs(ioUnit, aIDs)

    integer :: ioUnit
    integer, dimension( 3 ) :: aIDs

    logical :: stillPending
    character(256) :: iMsg


    DO i = 1, 3
        WRITE(0, *) "aIDs(", i, ") = '", aIDs( i ), "'"

        INQUIRE(43, id=aIDs( i ), pending=stillPending, IOMSG=iMsg, ERR=100)

        IF (i > 2) CALL zzrc( INT((40 + i), 4) )
        GOTO 200

100     WRITE(0, *) "INQUIRE(ERR=100,ID=", aIDs( i ), ") ", iMsg
        IF (i < 3) CALL zzrc( INT((44 + i), 4) )

200     CONTINUE
    END DO

end subroutine InquireOnIDs
