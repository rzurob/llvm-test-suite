!*  ===================================================================
!*
!*                               Specifier in I/O Statements
!*
!*  DATE                       : April  7, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pending Data Transfers on a Unit OPEN()ed
!*                               for Asynchronous I/O to a specific File
!*  SECONDARY FUNCTIONS TESTED : Subsequent OPEN() of a different File
!*                               using the same Unit
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : OPEN(), ASYNCHRONOUS= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  Wait Operations should be performed for each of the Pending Data
!*  Transfers.
!*
!*  9.4.5 The OPEN statement
!*
!*  If the file to be connected to the unit is not the same as the
!*  file to which the unit is connected, the effect is as if a CLOSE
!*  statement without a STATUS= specifier had been executed for the
!*  unit immediately prior to the execution of an OPEN statement.
!*
!*  9.4.6 The CLOSE statement
!*
!*  Execution of a CLOSE statement performs a wait operation for any
!*  pending asynchronous data transfer operations for the specified
!*  unit.
!*
!*  9.6.2 Wait operation
!*
!*  A wait operation completes the processing of a pending data transfer
!*  operation. Each wait operation completes only a single data transfer
!*  operation, although a single statement may perform multiple wait
!*  operations.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM asynchSpecOpen12

    INTEGER, PARAMETER :: n = 10000

    INTEGER, DIMENSION( n ) :: ioIDa
    INTEGER, DIMENSION( n ) :: ioIDb

    LOGICAL :: isPending

    CHARACTER(LEN = 256) :: iMsg


    OPEN(181, FILE='asynchSpecOpen12a.dat',&
            ACTION='write', ACCESS='stream', IOMSG=iMsg,&
            ASYNCHRONOUS='yes', FORM='unformatted', IOSTAT=iStat)
    if (0 <> iStat) then
        write(0, *) "OPEN(asynchSpecOpen12a.dat) <", iStat, "> ", iMsg
        call zzrc( 1 )
    end if


    DO i = 1, n
        WRITE(181, ASYNCHRONOUS='yes', ID=ioIDa( i ),&
            IOMSG=iMsg, IOSTAT=iStat) ((i * 137) + 65)

        if (0 <> iStat) then
            write(0, *) i, "WRITE(asynchSpecOpen12a.dat) <", iStat, "> ", iMsg
            call zzrc( 2 )
        end if
    END DO


    OPEN(181, FILE='asynchSpecOpen12b.dat',&
            ACTION='write', ACCESS='sequential', IOMSG=iMsg,&
            ASYNCHRONOUS='yes', FORM='unformatted', IOSTAT=iStat)
    if (0 <> iStat) then
        write(0, *) "OPEN(asynchSpecOpen12b.dat) <", iStat, "> ", iMsg
        call zzrc( 3 )
    end if


    DO i = 1, n
        if (MOD(i, 2) == 0) then
            WAIT(181, ID=ioIDa( i ), IOMSG=iMsg, IOSTAT=iStat)
            if (224 <> iStat) then
                write(0, *) i, "WAIT(asynchSpecOpen12a.dat,ID=",&
                                ioIDa( i ), ") <", iStat, "> ", iMsg
                call zzrc( 4 )
            end if

            WRITE(181, ASYNCHRONOUS='yes', ID=ioIDb( i ),&
                IOMSG=iMsg, IOSTAT=iStat) ((i * 137) + 65)

        else
            INQUIRE(181, ID=ioIDa( i ),&
                    PENDING=isPending, IOMSG=iMsg, IOSTAT=iStat)
            if (226 <> iStat) then
                write(0, *) i, "INQUIRE(asynchSpecOpen12a.dat,ID=",&
                                ioIDa( i ), ") <", iStat, "> ", iMsg
                call zzrc( 5 )

            else if ( isPending ) then
                write(0, *) i, "INQUIRE(asynchSpecOpen12a.dat,ID=",&
                                ioIDa( i ), ",PENDING=", isPending, ")"
                call zzrc( 6 )
            end if

            WRITE(181, IOMSG=iMsg, IOSTAT=iStat) ((i * 137) + 65)
        end if

        if (0 <> iStat) then
            write(0, *) i, "WRITE(asynchSpecOpen12b.dat) <", iStat, "> ", iMsg
            call zzrc( 6 )
        end if
    END DO


    CLOSE(181, IOMSG=iMsg, IOSTAT=iStat)
    if (0 <> iStat) then
        write(0, *) "CLOSE(asynchSpecOpen12b.dat) <", iStat, "> ", iMsg
        call zzrc( 7 )
    end if


    DO i = 1, n
        if (MOD(i, 2) == 0) then
            WAIT(181, ID=ioIDb( i ), IOMSG=iMsg, IOSTAT=iStat)
            if (224 <> iStat) then
                write(0, *) i, "WAIT(asynchSpecOpen12b.dat,ID=",&
                                ioIDb( i ), ") <", iStat, "> ", iMsg
                call zzrc( 8 )
            end if
        end if
    END DO

END PROGRAM asynchSpecOpen12
