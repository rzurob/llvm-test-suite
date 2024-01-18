!*  ===================================================================
!*
!*  DATE                       : March 21, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Perform Formatted Asynchronous I/O
!*                               Operations on several Units
!*  SECONDARY FUNCTIONS TESTED : Perform an INQUIRE() on each Pending
!*                               Data Transfer on a Unit other than the
!*                               Unit for which the Pending Data Transfer
!*                               was initiated
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

program pendingIDSpec04

    integer, dimension( 10 ) :: ioID
    integer, dimension( 10 ) :: ioUnit = (/ (i, i = 4, 94, 10) /)
    integer, dimension( 10 ) :: dataValues = (/ ((i * 567), i = 5, 95, 10) /)

    character(len = 256) :: iMsg


    do i = 1, 10
        if (mod(i, 2) == 0) then
            open(ioUnit( i ), asynchronous='yes',&
                &form='formatted', action='read', recl=6,&
                &access='direct', iostat=iStat, iomsg=iMsg)

            if (0 /= iStat) then
                write(0, *) "OPEN(", ioUnit( i ), ") <", iStat, "> ", iMsg
                call zzrc( i )
            end if

        else
            open(ioUnit( i ), asynchronous='yes',&
                &form='formatted', action='write', recl=6,&
                &access='direct', iostat=iStat, iomsg=iMsg)

            if (0 /= iStat) then
                write(0, *) "OPEN(", ioUnit( i ), ") <", iStat, "> ", iMsg
                call zzrc( i )
            end if
        end if
    end do


    do i = 10, 1, -2
        read(ioUnit( i ), asynchronous='yes', rec=i, iomsg=iMsg,&
                &FMT=100, id=ioID( i ), iostat=iStat) dataValues( i )

        if (0 /= iStat) then
            write(0, *) "READ(", ioUnit( i ), ") <", iStat, "> ", iMsg
            call zzrc( (10 + i) )
        end if
    end do

100 format(i6)

    do i = 1, 9, 2
        write(ioUnit( i ), asynchronous='yes', rec=i, iomsg=iMsg,&
                &FMT=100, id=ioID( i ), iostat=iStat) dataValues( i )

        if (0 /= iStat) then
            write(0, *) "WRITE(", ioUnit( i ), ") <", iStat, "> ", iMsg
            call zzrc( (20 + i) )
        end if
    end do


    call CheckIDs(ioUnit, ioID)


    do i = 1, 10
        close(ioUnit( i ), iostat=iStat, iomsg=iMsg)

        if (0 /= iStat) then
            write(0, *) "CLOSE(", ioUnit( i ), ") <", iStat, "> ", iMsg
            call zzrc( i )
        end if
    end do


    print '(10i6)', (dataValues( i ), i = 1, 10)


    contains


        subroutine CheckIDs(unitList, aIDs)
            integer, dimension( 10 ) :: unitList
            integer, dimension( 10 ) :: aIDs

            logical :: isPending

            character(len = 256) :: iMsg


            do i = 1, 10
                j = 11 - i
                inquire(unitList( i ), id=aIDs( j ),&
                        &pending=isPending, iostat=iStat, iomsg=iMsg)

                if (0 /= iStat) then
                    write(0, *) "INQUIRE(", unitList( i ), ",ID=",&
                                &aIDs( j ), ") <", iStat, "> ", iMsg
                    call zzrc( (30 + i) )

                else if ( isPending ) then
                    write(0, *) "INQUIRE(", unitList( i ), ",ID=",&
                                &aIDs( j ), ",PENDING=", isPending, ")"
                    call zzrc( (40 + i) )
                end if
            end do

        end subroutine CheckIDs

end program pendingIDSpec04
