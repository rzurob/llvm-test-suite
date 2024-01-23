!*  ===================================================================
!*
!*  DATE                       : March 29, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pending Data Transfers with an Error Condition
!*  SECONDARY FUNCTIONS TESTED : INQUIRE() with the PENDING=, and ERR=
!*                               Specifiers; the IOSTAT= Specifier is *NOT*
!*                               present
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : INQUIRE(), PENDING= Specifier, ERR=
!*                               Specifier
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
!*                     or  ERR = label
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

PROGRAM pendingNoIOSTATSpec02d

    integer, dimension( 10 ) :: dataValues

    integer, dimension( 10 ) :: recNum = (/ 10, 9, 8, 7, 66, 1, 2, 3, 4, 5 /)

    logical :: sumPending

    character(len = 256) :: iMsg


    open(44, ASYNCHRONOUS='yes', ACCESS='direct',&
            RECL=4, ACTION='readwrite', IOSTAT=iStat, IOMSG=iMsg)
    if (iStat <> 0) then
        write(0, *) "OPEN() <", iStat, "> ", iMsg
        error stop 11
    end if


    do i = 10, 1, -1
        write(44, ASYNCHRONOUS='yes', REC=i,&
                IOSTAT=iStat, IOMSG=iMsg) ((i * 56) + 23)

        if (iStat <> 0) then
            write(0, *) i, "WRITE() <", iStat, "> ", iMsg
            call zzrc( (i + 20) )
        end if
    end do


    inquire(44, PENDING=sumPending, ERR=100, IOSTAT=iStat, IOMSG=iMsg)
    if (iStat <> 0) then
        write(0, *) "INQUIRE() <", iStat, "> ", iMsg
        error stop 31
    end if

    goto 200

100 write(0, *) "INQUIRE(ERR=100) <", iStat, "> ", iMsg
    error stop 51


200 do i = 1, 10
        read(44, ASYNCHRONOUS='yes', REC=recNum( i ),&
                IOSTAT=iStat, IOMSG=iMsg) dataValues( i )

        if (iStat <> 0) then
            write(0, *) i, "READ() <", iStat, "> ", iMsg
            call zzrc( (i + 60) )
        end if
    end do


    inquire(44, PENDING=sumPending, ERR=300, IOMSG=iMsg)

    write(0, *) "INQUIRE() ", iMsg
    error stop 71

300 write(0, *) "INQUIRE(ERR=300) ", iMsg


400 close(44, IOSTAT=iStat, IOMSG=iMsg)
    if (iStat <> 0) then
        write(0, *) "CLOSE() <", iStat, "> ", iMsg
        error stop 91
    end if

END PROGRAM pendingNoIOSTATSpec02d
