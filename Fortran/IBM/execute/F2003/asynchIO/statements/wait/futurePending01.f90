!*  ===================================================================
!*
!*  DATE                       : April 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pending Data Transfers
!*  SECONDARY FUNCTIONS TESTED : WAIT() with ID= Specifier Values of
!*                               Future Pending Data Transfers
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WRITE(), WAIT(), ID= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  9.6.1 WAIT statement
!*
!*  A WAIT statement performs a wait operation for specified pending
!*  asynchronous data transfer operations.
!*
!*  NOTE 9.51
!*  The CLOSE, INQUIRE, and file positioning statements may also perform wait
!*  operations.
!*
!*  R921 wait-stmt   is  WAIT (wait-spec-list)
!*  R922 wait-spec   is  [ UNIT = ] file-unit-number
!*                   or  END = label
!*                   or  EOR = label
!*                   or  ERR = label
!*                   or  ID = scalar-int-expr
!*  ...
!*
!*  9.6.2 Wait operation
!*
!*  A wait operation completes the processing of a pending data transfer
!*  operation. Each wait operation completes only a single data transfer
!*  operation, although a single statement may perform multiple wait
!*  operations.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM futurePending01

    INTERFACE
        INTEGER FUNCTION Wait4IDs(ioUnit, n, aIDs)
            integer, INTENT(in) :: ioUnit
            integer, INTENT(in) :: n
            integer, dimension( n ), INTENT(in) :: aIDs
        END FUNCTION Wait4IDs
    END INTERFACE

    integer, parameter :: n = 10000

    integer, dimension( n ) :: ioID
    integer, dimension( n ) :: dataValues = (/ (((233 * i) + 127), i = 1, n) /)

    character(len = 256) :: iMsg = ''


    open(9, access='stream', asynchronous='yes',&
        action='write', form='unformatted', iostat=iStat, iomsg=iMsg)
    if (iStat /= 0) then
        write(0, *) "OPEN() <", iStat, "> ", iMsg
        call zzrc( 1 )
    end if


    do i = 1, n
        write(9, asynchronous='yes', id=ioID( i ),&
                iostat=iStat, iomsg=iMsg) dataValues( i )

        if (iStat /= 0) then
            write(0, *) i, ") WRITE() <", iStat, "> ", iMsg
            call zzrc( 2 )
        end if


        idFuture = ioID( i ) + 1
        wait(9, id=idFuture, iostat=iStat, iomsg=iMsg)
        if (iStat /= 224) then
            write(0, *) i, ") WAIT(ID=", idFuture, ") <", iStat, "> ", iMsg
            call zzrc( 3 )
        end if
    end do


    iStat = Wait4IDs(9, n, ioID)
    if (iStat /= 0) then
        call zzrc( 4 )
    end if


    close(9, iostat=iStat, iomsg=iMsg)
    if (iStat /= 0) then
        write(0, *) "CLOSE() <", iStat, "> ", iMsg
        call zzrc( 5 )
    end if

END PROGRAM futurePending01


INTEGER FUNCTION Wait4IDs(ioUnit, n, aIDs)
    integer, INTENT(in) :: ioUnit
    integer, INTENT(in) :: n
    integer, dimension( n ), INTENT(in) :: aIDs

    character(len = 256) :: iMsg = ''

    i = 0
    iStat = 0

    do while ((i < n)  .AND.  (iStat == 0))
        i = i + 1

        wait(9, id=aIDs( i ), iostat=iStat, iomsg=iMsg)
        if (iStat /= 0) then
            write(0, *) i, ") WAIT(ID=", aIDs( i ), ") <", iStat, "> ", iMsg
        end if
    end do


    Wait4IDs = iStat

END FUNCTION Wait4IDs
