!*  ===================================================================
!*
!*  DATE                       : March 28, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pending Unformatted Data Transfers
!*  SECONDARY FUNCTIONS TESTED : Synchronous Data Transfer Initiated; Wait
!*                               Operations should be performed for the
!*                               Pending Data Transfers
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WRITE(), ASYNCHRONOUS= Specifier, ID=
!*                               Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                :
!*
!*  9.5 Data transfer statements
!*
!*  R910 read-stmt        is  READ ( io-control-spec-list )[ input-item-list ]
!*  R911 write-stmt       is  WRITE ( io-control-spec-list )[ output-item-list ]
!*
!*  9.5.1 Control information list
!*
!*  R913 io-control-spec  is  [ UNIT = ] io-unit
!*                        or  [ FMT = ] format
!*                        or  [ NML = ] namelist-group-name
!*                        or  ADVANCE = scalar-default-char-expr
!*                        or  ASYNCHRONOUS = scalar-char-initialization-expr
!*  ...
!*                        or  END = label
!*                        or  EOR = label
!*                        or  ERR = label
!*                        or  ID = scalar-int-variable
!*                        or  IOMSG = iomsg-variable
!*                        or  IOSTAT = scalar-int-variable
!*  ...
!*                        or  SIZE = scalar-int-variable
!*
!*  9.5.3 Execution of a data transfer input/output statement
!*
!*  The effect of executing a synchronous data transfer input/output
!*  statement shall be as if the following operations were performed in the
!*  order specified:
!*
!*  ...
!*
!*  (3) Perform a wait operation for all pending input/output operations for
!*      the unit. If an error, end-of-file, or end-of-record condition occurs
!*      during any of the wait operations, steps 4 through 8 are skipped for
!*      the current data transfer statement.
!*
!*  9.6.2 Wait operation
!*
!*  A wait operation completes the processing of a pending data transfer
!*  operation.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM pendingSynchRead01

    interface
        SUBROUTINE Wait4ID(ioUnit, aID, failRCbase)
            INTEGER, intent(in)  :: ioUnit
            INTEGER, DIMENSION( 3 ), intent(in) :: aID
            INTEGER, intent(in)  :: failRCbase
        END SUBROUTINE Wait4ID
    end interface

    INTEGER, DIMENSION( 3 ) :: ioID

    CHARACTER(LEN = 256) :: iMsg = ''


    open(100, ACCESS='sequential', ASYNCHRONOUS='yes',&
                    ACTION='write', FORM='unformatted')


    do i = 1, 3
        write(100, ASYNCHRONOUS='yes', ID=ioID( i )) ((i * 42) + 7)
    end do


    write(100, IOSTAT=iStat, IOMSG=iMsg) ((4 * 42) + 7)
    if (0 <> iStat) then
        write(0, *) "WRITE() <", iStat, "> ", iMsg
        error stop 11
    end if


    call Wait4ID(100, ioID, 20)


    do i = 1, 3
        write(100, ASYNCHRONOUS='yes', ID=ioID( i )) (((i + 4) * 42) + 7)
    end do


    write(100, ASYNCHRONOUS='no', IOSTAT=iStat, IOMSG=iMsg) ((8 * 42) + 7)
    if (0 <> iStat) then
        write(0, *) "WRITE(ASYNCHRONOUS=no) <", iStat, "> ", iMsg
        error stop 31
    end if


    call Wait4ID(100, ioID, 40)


    close( 100 )

END PROGRAM pendingSynchRead01


SUBROUTINE Wait4ID(ioUnit, aID, failRCbase)

    INTEGER, intent(in)  :: ioUnit
    INTEGER, DIMENSION( 3 ), intent(in) :: aID
    INTEGER, intent(in)  :: failRCbase

    CHARACTER(LEN = 256) :: iMsg = ''


    do i = 3, 1, -1
        wait(ioUnit, ID=aID( i ), IOSTAT=iStat, IOMSG=iMsg)
        if (224 <> iStat) then
            write(0, *) "WAIT(", aID( i ), ") <", iStat, "> ", iMsg
            call zzrc( failRCbase + i )
        end if
    end do

END SUBROUTINE Wait4ID
