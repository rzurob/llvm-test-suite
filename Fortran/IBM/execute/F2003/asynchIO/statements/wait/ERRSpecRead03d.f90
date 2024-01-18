!*  ===================================================================
!*
!*  DATE                       : March 31, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Unformatted Pending Data Transfers with
!*                               an end-of-file Condition
!*  SECONDARY FUNCTIONS TESTED : WAIT() with the ID= Specifier; IOSTAT=, END=,
!*                               and ERR= Specifiers are *NOT* present
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WAIT(), ID= Specifier
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
!*  R921 wait-stmt  is  WAIT (wait-spec-list)
!*  R922 wait-spec  is  [ UNIT = ] file-unit-number
!*                  or  END = label
!*                  or  EOR = label
!*                  or  ERR = label
!*                  or  ID = scalar-int-expr
!*                  or  IOMSG = iomsg-variable
!*                  or  IOSTAT = scalar-int-variable
!*
!*  ...
!*
!*  If any error, end-of-file, or end-of-record conditions occur, the
!*  applicable actions specified by the IOSTAT=, IOMSG=, ERR=, END=,
!*  and EOR= specifiers of the statement that performs the wait oper-
!*  ation are taken.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM ERRSpecRead03d

    integer, dimension( 3 ) :: ioID
    integer, dimension( 3 ) :: dataValue

    character(len = 256) :: iMsg


    open(938, ACTION='read', ACCESS='stream',&
        FILE='ERRSpecRead03d.dat', IOMSG=iMsg,&
        FORM='unformatted', ASYNCHRONOUS='yes', IOSTAT=iStat)
    if (iStat <> 0) then
        write(0, *) "OPEN() <", iStat, "> ", iMsg
        call zzrc( 11 )
    end if


    do i = 1, 3
        read(938, ASYNCHRONOUS='yes', ID=ioID( i ),&
                IOSTAT=iStat, IOMSG=iMsg) dataValue( i )

        if (iStat <> 0) then
            write(0, *) i, ") READ() <", iStat, "> ", iMsg
            call zzrc( 20 + i )
        end if
    end do


    call Wait4ID(938, ioID)

    write(0, '(3I5)') (dataValue( i ), i = 1, 3)


    close(938, IOMSG=iMsg, IOSTAT=iStat)
    if (iStat <> 0) then
        write(0, *) "CLOSE() <", iStat, "> ", iMsg
        call zzrc( 31 )
    end if

END PROGRAM ERRSpecRead03d


SUBROUTINE Wait4ID(ioUnit, aIDs)

    integer, intent(in) :: ioUnit
    integer, dimension( 3 ), intent(in) :: aIDs


    do i = 1, 3
        write(0, *) i, ") aIDs(", i, ") =", aIDs( i )

        wait(ioUnit, ID=aIDs( i ))
    end do

END SUBROUTINE Wait4ID
