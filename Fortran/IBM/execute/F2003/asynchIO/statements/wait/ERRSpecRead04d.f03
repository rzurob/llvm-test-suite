!*  ===================================================================
!*
!*  DATE                       : March 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Unformatted Pending Data Transfers with
!*                               an end-of-file Condition
!*  SECONDARY FUNCTIONS TESTED : WAIT() without the ID= Specifier; IOSTAT=,
!*                               END=, and ERR= Specifiers are *NOT* present
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WAIT()
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

PROGRAM ERRSpecRead04d

    INTEGER, DIMENSION( 3 ) :: dataValue

    CHARACTER(len = 256) :: iMsg


    open(939, FORM='unformatted', ACCESS='direct', RECL=4,&
        ASYNCHRONOUS='yes', ACTION='read', IOSTAT=iStat, IOMSG=iMsg)
    if (iStat <> 0) then
        write(0, *) "OPEN() <", iStat, "> ", iMsg
        call zzrc( 11 )
    end if


    do i = 3, 1, -1
        read(939, REC=i, ASYNCHRONOUS='yes',&
            IOSTAT=iStat, IOMSG=iMsg) dataValue( i )

        if (iStat <> 0) then
            write(0, *) i, ") READ() <", iStat, "> ", iMsg
            call zzrc( (20 + i) )
        end if

        write(0, *) "READ(REC=", i, ")"
    end do


    wait( 939 )

    write(0, '(3I5)') (dataValue( i ), i = 1, 3)


    close(939, IOSTAT=iStat, IOMSG=iMsg)
    if (iStat <> 0) then
        write(0, *) "CLOSE() <", iStat, "> ", iMsg
        call zzrc( 11 )
    end if

END PROGRAM ERRSpecRead04d
