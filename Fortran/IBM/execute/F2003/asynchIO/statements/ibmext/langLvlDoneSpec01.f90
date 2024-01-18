!*  ===================================================================
!*
!*                               Language Level Options
!*
!*  DATE                       : April 11, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : -qlanglvl=[extended|2003pure|2003std] Option
!*  SECONDARY FUNCTIONS TESTED : DONE= Specifier in the WAIT() Statement
!*
!*                               langLvlDoneSpec01p:  xlf95
!*                               langLvlDoneSpec01s:  xlf95
!*  REQUIRED COMPILER OPTIONS  : langLvlDoneSpec01e:  -qlanglvl=extended
!*                               langLvlDoneSpec01p:  -qlanglvl=2003pure
!*                               langLvlDoneSpec01s:  -qlanglvl=2003std
!*
!*  KEYWORD(S)                 : WAIT(), DONE= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  Code which uses the IBM Extension(s) noted below.
!*
!*  WAIT
!*
!*         ----------    IBM  Extension    ----------
!*    Purpose
!*      The WAIT statement may be used to wait for an asynchronous data
!*      transfer to complete or it may be used to detect the completion
!*      status of an asynchronous data transfer statement.
!*
!*      Syntax
!*
!*        WAIT ( wait_list )
!*
!*        wait_list
!*          is a list that must contain one ID= specifier and at most one
!*          of each of the other valid specifiers. The valid specifiers are:
!*
!*        DONE= logical_variable
!*          specifies whether or not the asynchronous I/O statement is
!*          complete.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM langLvlDoneSpec01

    type tPendingDataXfer
        integer id
        logical isDone
    end type tPendingDataXfer

    type(tPendingDataXfer), dimension( 10 ) :: pendingDataXfers =&
                        (/ (tPendingDataXfer(0,.FALSE.), i = 1, 10) /)

    integer, dimension( 100 ) :: dataValues

    character(len = 256) :: iMsg


    open(420, ASYNCHRONOUS='yes', FORM='unformatted',&
        ACCESS='stream', ACTION='read', IOSTAT=iStat, IOMSG=iMsg)
    if (iStat /= 0) then
        write(0, *) "OPEN() <", iStat, "> ", iMsg
        call zzrc( 1 )
    end if


    do i = 10, 1, -1
        j = (i - 1) * 10 + 1

        read(420, ASYNCHRONOUS='yes', ID=pendingDataXfers( i )%id,&
            IOSTAT=iStat, IOMSG=iMsg) (dataValues( k ), k = j, (j + 9))

        if (iStat /= 0) then
            write(0, *) i, "READ() <", iStat, "> ", iMsg
            call zzrc( 2 )
        end if
    end do


    !
    !  IBM Extension:  "DONE=" Specifier.
    !
    numDone = 0
    do while (numDone < 10)
        do i = 1, 10
            if (.NOT. pendingDataXfers( i )%isDone) then
                wait(420, ID=pendingDataXfers( i )%id, IOMSG=iMsg,&
                    DONE=pendingDataXfers( i )%isDone, IOSTAT=iStat)

                if (iStat /= 0) then
                    write(0, *) i, "WAIT() <", iStat, "> ", iMsg
                    call zzrc( 3 )

                else if ( pendingDataXfers( i )%isDone ) then
                    numDone = numDone + 1
                end if
            end if
        end do
    end do


    do i = 1, 100, 10
        write(6, '(10I6)') (dataValues( j ), j = i, (i + 9))
    end do


    close(420, IOSTAT=iStat, IOMSG=iMsg)
    if (iStat /= 0) then
        write(0, *) "CLOSE() <", iStat, "> ", iMsg
        call zzrc( 5 )
    end if

END PROGRAM langLvlDoneSpec01
