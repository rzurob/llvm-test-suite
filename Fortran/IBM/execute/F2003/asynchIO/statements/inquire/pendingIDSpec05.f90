!*  ===================================================================
!*
!*  DATE                       : March 21, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Perform Unformatted Asynchronous I/O
!*                               Operations on a Unit
!*  SECONDARY FUNCTIONS TESTED : Perform an INQUIRE() with the PENDING=
!*                               Specifier ID= Specifier on Specific
!*                               Pending Data Transfers
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ(), WRITE(), INQUIRE(), PENDING=
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

program pendingIDSpec05

    INTEGER, DIMENSION( 2,10 ) :: ioID

    INTEGER, DIMENSION( 10 ) :: dataIn
    INTEGER, DIMENSION( 10 ) :: dataOut = (/ ((356 * i), i = 1, 10) /)

    LOGICAL :: rePending
    LOGICAL, DIMENSION( 2,10 ) :: unPending

    CHARACTER(LEN = 5), DIMENSION( 2 ) :: actType = (/ 'read ', 'write' /)

    CHARACTER(LEN = 256) :: iMsg


    do i = 1, 2
        OPEN((10 + i), ASYNCHRONOUS='yes', ACTION=actType( i ),&
            &ACCESS='stream', FORM='unformatted', IOSTAT=iStat, IOMSG=iMsg)

        if (iStat /= 0) then
            WRITE(0, *) i, ") OPEN(", actType( i ), ") <", iStat, "> ", iMsg
            call zzrc( i )
        end if
    end do


    do i = 1, 10
        do j = 1, 2
            if (actType( j ) == 'read') then
                READ((10 + j), ASYNCHRONOUS='yes', ID=ioID( j,i ),&
                                &IOSTAT=iStat, IOMSG=iMsg) dataIn( i )

            else
                WRITE((10 + j), ASYNCHRONOUS='yes', ID=ioID( j,i ),&
                                &IOSTAT=iStat, IOMSG=iMsg) dataOut( i )
            end if

            if (iStat /= 0) then
                WRITE(0, *) i, ") ", actType( j ), "() <", iStat, "> ", iMsg
                call zzrc( j * 10 + i )
            end if
        end do
    end do


    unPending = .TRUE.
    do i = 2, 1, -1
        INQUIRE((10 + i), ID=ioID( i,5 ),&
            &PENDING=unPending( i,5 ), IOSTAT=iStat, IOMSG=iMsg)

        if (iStat /= 0) then
            WRITE(0, *) i, ") INQUIRE(", actType( i ), ",ID=",&
                            &ioID( i,5 ), ") <", iStat, "> ", iMsg
            call zzrc( i + 30 )
        end if
    end do


    do i = 10, 1, -1
        do j = 2, 1, -1
            INQUIRE((10 + j), ID=ioID( j,i ),&
                &PENDING=rePending, IOSTAT=iStat, IOMSG=iMsg)

            if ( unPending( j,i ) ) then
                if (iStat /= 0) then
                    WRITE(0, *) i, ") INQUIRE(", actType( j ), ",ID=",&
                                    &ioID( j,i ), ") <", iStat, "> ", iMsg
                    call zzrc( i + 40 )
                end if

            else
                if (iStat /= 226) then
                    WRITE(0, *) i, ") INQUIRE(", actType( j ), ",ID=",&
                                    &ioID( j,i ), ") <", iStat, "> ", iMsg
                    call zzrc( i + 50 )

                else if ( rePending ) then
                    WRITE(0, *) i, ") INQUIRE(", actType( j ), ",ID=",&
                                &ioID( j,i ), ",PENDING=", unPending, ")"
                    call zzrc( i + 60 )
                end if
            end if
        end do
    end do


    do i = 1, 2
        CLOSE((10 + i), IOSTAT=iStat, IOMSG=iMsg)

        if (iStat /= 0) then
            WRITE(0, *) i, ") CLOSE(", actType( i ), ") <", iStat, "> ", iMsg
            call zzrc( 71 )
        end if
    end do


    do i = 1, 10
        if (dataOut( i ) /= dataIn( i )) then
            WRITE(0, *) " dataIn(", i, ") = '", dataIn( i ), "'"
            WRITE(0, *) "dataOut(", i, ") = '", dataOut( i ), "'"

            call zzrc( i + 80 )
        end if
    end do

end program pendingIDSpec05
