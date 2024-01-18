!*  ===================================================================
!*
!*  DATE                       : April 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pending Data Transfers with an Error
!*                               Condition
!*  SECONDARY FUNCTIONS TESTED : CLOSE() with the ERR= Specifier;  both
!*                               with and without the IOSTAT= Specifier
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : CLOSE(), ERR= Specifier, IOSTAT= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                :
!*
!*  9.4.6 The CLOSE statement
!*
!*  The CLOSE statement is used to terminate the connection of a specified
!*  unit to an external file.
!*
!*  Execution of a CLOSE statement performs a wait operation for any pending
!*  asynchronous data transfer operations for the specified unit.
!*
!*  ...
!*
!*  R908 close-stmt  is  CLOSE ( close-spec-list )
!*  R909 close-spec  is  [ UNIT = ] file-unit-number
!*                   or  IOSTAT = scalar-int-variable
!*                   or  IOMSG = iomsg-variable
!*                   or  ERR = label
!*                   or  STATUS = scalar-default-char-expr
!*
!*  9.6.1 WAIT statement
!*
!*  NOTE 9.51
!*  The CLOSE, INQUIRE, and file positioning statements may also perform wait
!*  operations.
!*
!*  9.6.2 Wait operation
!*
!*  A wait operation completes the processing of a pending data transfer
!*  operation. Each wait operation completes only a single data transfer
!*  operation, although a single statement may perform multiple wait
!*  operations.
!*
!*  If an error or end-of-file condition occurs during a wait operation
!*  for a unit, the processor performs a wait operation for all pending
!*  data transfer operations for that unit.
!*
!*  NOTE 9.53
!*  Error, end-of-file, and end-of-record conditions may be raised either
!*  during the data transfer statement that initiates asynchronous
!*  input/output, a subsequent asynchronous data transfer statement for
!*  the same unit, or during the wait operation. ...  if they are raised
!*  during the wait operation, the actions are in accordance with the
!*  specifiers of the statement that performs the wait operation.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM pendingClose03

    integer, dimension( 5 ) :: ioID
    integer, dimension( 5 ) :: dataValues = 0

    character(len = 256) :: iMsg
    character(len = 30) :: fileName = 'pendingClose03.dat'


    open(24, file=fileName, action='read', recl=4,&
        access='direct', asynchronous='yes', form='unformatted')

    do i = 1, 5
        read(24, rec=i, asynchronous='yes', id=ioID( i )) dataValues( i )
    end do


    close(24, ERR=100, iostat=iStat, iomsg=iMsg)

    write(0, *) "CLOSE() <", iStat, "> ", iMsg
    call zzrc( 11 )

100 write(0, *) "CLOSE(ERR=100) <", iStat, "> ", iMsg
    if (iStat /= 1) then
        call zzrc( 12 )
    end if


    do i = 1, 5
        wait(24, iomsg=iMsg, id=ioID( i ), iostat=iStat)

        if (iStat /= 224) then
            write(0, *) i, "WAIT(ID=", ioID( i ), ") <", iStat, "> ", iMsg
            call zzrc( (i + 20) )
        end if
    end do

    write(0, '("ERR=/IOSTAT=: ",5I5)') (dataValues( i ), i = 1, 5)


    open(24, file=fileName, action='read', recl=4,&
        access='direct', asynchronous='yes', form='unformatted')

    do i = 1, 5
        read(24, rec=i, asynchronous='yes', id=ioID( i )) dataValues( i )
    end do


    close(24, ERR=200, iomsg=iMsg)

    write(0, *) "CLOSE()", iMsg
    call zzrc( 31 )

200 write(0, *) "CLOSE(ERR=100)", iMsg


    do i = 1, 5
        wait(24, iomsg=iMsg, id=ioID( i ), iostat=iStat)

        if (iStat /= 224) then
            write(0, *) i, "WAIT(ID=", ioID( i ), ") <", iStat, "> ", iMsg
            call zzrc( (i + 40) )
        end if
    end do

    write(0, '("   ERR= Only: ",5I5)') (dataValues( i ), i = 1, 5)

END PROGRAM pendingClose03
