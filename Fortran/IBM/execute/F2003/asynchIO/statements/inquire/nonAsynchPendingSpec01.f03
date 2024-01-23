!*  ===================================================================
!*
!*  DATE                       : March 21, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : INQUIRE() on a Unit not Opened for
!*                               Asynchronous I/O
!*  SECONDARY FUNCTIONS TESTED : PENDING= and ID= Specifiers are present
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ()/WRITE(), INQUIRE(), PENDING=
!*                               Specifier, ID= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
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
!*  If an ID= specifier appears and the specified data transfer operation
!*  is complete, then the variable specified in the PENDING= specifier is
!*  assigned the value false and the INQUIRE statement performs the wait
!*  operation for the specified data transfer.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program nonAsynchPendingSpec01

    logical :: exPending

    character(len = 256) :: iMsg


    open(2, asynchronous='yes', action='write',&
        &access='stream', form='unformatted', iostat=iStat, iomsg=iMsg)
    if (0 /= iStat) then
        write(0, *) "OPEN(ASYNCHRONOUS=yes) <", iStat, "> ", iMsg
        error stop 1
    end if

    open(3, action='write', access='stream',&
        &form='unformatted', iostat=iStat, iomsg=iMsg)
    if (0 /= iStat) then
        write(0, *) "OPEN(ASYNCHRONOUS=no) <", iStat, "> ", iMsg
        error stop 2
    end if


    write(3, iostat=iStat, iomsg=iMsg) 1234
    if (0 /= iStat) then
        write(0, *) "WRITE(ASYNCHRONOUS=no) <", iStat, "> ", iMsg
        error stop 3
    end if

    write(2, asynchronous='yes', id=ioID, iostat=iStat, iomsg=iMsg) 1234
    if (0 /= iStat) then
        write(0, *) "WRITE(ASYNCHRONOUS=yes) <", iStat, "> ", iMsg
        error stop 4
    end if


    inquire(3, id=ioID, pending=exPending, iostat=iStat, iomsg=iMsg)
    if (227 /= iStat) then
        write(0, *) "INQUIRE(ASYNCHRONOUS=no,ID=", ioID,&
                                    &") <", iStat, "> ", iMsg
        error stop 5

    else if ( exPending ) then
        write(0, *) "INQUIRE(ASYNCHRONOUS=no,ID=", ioID,&
                                &",PENDING=", exPending, ")"
        error stop 6
    end if


    inquire(3, pending=exPending, iostat=iStat, iomsg=iMsg)
    if (0 /= iStat) then
        write(0, *) "INQUIRE(ASYNCHRONOUS=no) <", iStat, "> ", iMsg
        error stop 7

    else if ( exPending ) then
        write(0, *) "INQUIRE(ASYNCHRONOUS=no,PENDING=", exPending, ")"
        error stop 8
    end if


    inquire(2, id=ioID, pending=exPending, iostat=iStat, iomsg=iMsg)
    if (0 /= iStat) then
        write(0, *) "INQUIRE(ASYNCHRONOUS=yes,ID=", ioID,&
                                    &") <", iStat, "> ", iMsg
        error stop 9
    end if


    close(2, iostat=iStat, iomsg=iMsg)
    if (0 /= iStat) then
        write(0, *) "CLOSE(ASYNCHRONOUS=yes) <", iStat, "> ", iMsg
        error stop 11
    end if

    close(3, iostat=iStat, iomsg=iMsg)
    if (0 /= iStat) then
        write(0, *) "CLOSE(ASYNCHRONOUS=no) <", iStat, "> ", iMsg
        error stop 12
    end if

end program nonAsynchPendingSpec01
