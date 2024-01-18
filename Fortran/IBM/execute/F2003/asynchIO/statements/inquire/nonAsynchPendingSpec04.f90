!*  ===================================================================
!*
!*  DATE                       : March 23, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Pending Data Transfer
!*  SECONDARY FUNCTIONS TESTED : INQUIRE() with the FILE= Specifier, PENDING=
!*                               and ID= Specifiers
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : INQUIRE(), FILE= Specifier, PENDING=
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
!*  If an ID= specifier appears and the specified data transfer operation
!*  is complete, then the variable specified in the PENDING= specifier is
!*  assigned the value false and the INQUIRE statement performs the wait
!*  operation for the specified data transfer.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program nonAsynchPendingSpec04

    logical :: dePending
    logical :: imPending

    character(len = 256) :: iMsg


    open(456, file='nonAsynchPendingSpec04.dat', iostat=iStat,&
        &iomsg=iMsg, asynchronous='yes', action='write', form='unformatted')
    if (0 /= iStat) then
        write(0, *) "OPEN() <", iStat, "> ", iMsg
        call zzrc( 1 )
    end if


    write(456, iostat=iStat, iomsg=iMsg, id=ioID, asynchronous='yes') 1234
    if (0 /= iStat) then
        write(0, *) "WRITE() <", iStat, "> ", iMsg
        call zzrc( 2 )
    end if


    inquire(file='nonAsynchPendingSpec04.dat',&
            &pending=dePending, iostat=iStat, iomsg=iMsg)
    if (0 /= iStat) then
        write(0, *) "INQUIRE() <", iStat, "> ", iMsg
        call zzrc( 3 )
    end if


    inquire(file='nonAsynchPendingSpec04.dat', id=ioID,&
            &pending=imPending, iostat=iStat, iomsg=iMsg)
    if (( dePending )  .AND.  (0 /= iStat)) then
        write(0, *) "INQUIRE(ID=", ioID, ") <", iStat, "> ", iMsg
        call zzrc( 5 )

    else if (.NOT. depending) then
        if (226 /= iStat) then
            write(0, *) "INQUIRE(ID=", ioID, ") <", iStat, "> ", iMsg
            call zzrc( 5 )

        else if ( imPending ) then
            write(0, *) "INQUIRE(ID=", ioID, ",PENDING=", imPending, ")"
            call zzrc( 5 )
        end if
    end if


    close(456, iostat=iStat, iomsg=iMsg)
    if (0 /= iStat) then
        write(0, *) "CLOSE() <", iStat, "> ", iMsg
        call zzrc( 7 )
    end if

end program nonAsynchPendingSpec04
