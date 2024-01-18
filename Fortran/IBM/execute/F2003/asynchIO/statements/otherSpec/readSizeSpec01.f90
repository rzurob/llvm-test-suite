!*  ===================================================================
!*
!*  DATE                       : March 28, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Formatted Non-Advancing Input Asynchronous
!*                               Data Transfers
!*  SECONDARY FUNCTIONS TESTED : SIZE= Specifier contains the Count of
!*                               Characters Transferred
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ(), ASYNCHRONOUS=, ID=, IOSTAT=, and
!*                               IOMSG= Specifiers
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 3
!*
!*  DESCRIPTION                :
!*  Formatted Asynchronous I/O.
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
!*  C921 (R913) An ADVANCE= specifier may appear only in a formatted
!*              sequential or stream input/output statement with explicit
!*              format specification (10.1) whose control information list
!*              does not contain an internal-file-variable as the io-unit.
!*
!*  C923 (R913) If a SIZE= specifier appears, an ADVANCE= specifier also
!*              shall appear.
!*
!*  9.5.1.14 SIZE= specifier in a data transfer statement
!*
!*  When a synchronous nonadvancing input statement terminates, the variable
!*  specified in the SIZE= specifier becomes defined with the count of the
!*  characters transferred by data edit descriptors during execution of the
!*  current input statement. ...
!*
!*  For asynchronous nonadvancing input, the storage units specified in
!*  the SIZE= specifier become defined with the count of the characters
!*  transferred when the corresponding wait operation is executed.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM readSizeSpec01

    integer, dimension( 10 ) :: ioID
    integer, dimension( 10 ) :: xferSize
    integer, dimension( 10 ) :: dataValue

    character(len = 256) :: iMsg


    open(73, ASYNCHRONOUS='yes', ACCESS='stream',&
        FILE='readSizeSpec01.dat', FORM='formatted', ACTION='read')


    do i = 1, 10
        read(73, '(I5)', ADVANCE='no',&
            ASYNCHRONOUS='yes', ID=ioID( i ),&
            SIZE=xferSize( i ), IOSTAT=iStat, IOMSG=iMsg) dataValue( i )

        if (0 <> iStat) then
            write(0, *) i, "READ() <", iStat, "> ", iMsg
            call zzrc( i + 10 )
        end if
    end do


    call Wait4ID(73, ioID, 20)


    do i = 10, 1, -1
        if (xferSize( i ) <> 5) then
            write(0, *) "xferSize(", i, ") = '", xferSize( i ), "'"
            write(0, *) "Should be 5!"

            call zzrc( i + 30 )
        end if
    end do


    write(6, '(10I6)') (dataValue( i ), i = 1, 10)


    close( 73 )

END PROGRAM readSizeSpec01


SUBROUTINE Wait4ID(ioUnit, aID, failRCbase)

    integer, intent(in) :: ioUnit
    integer, dimension( 10 ), intent(in) :: aID
    integer, intent(in) :: failRCbase

    character(len = 256) :: iMsg


    do i = 1, 10
        wait(ioUnit, ID=aID( i ), IOSTAT=iStat, IOMSG=iMsg)

        if (0 <> iStat) then
            write(0, *) i, "WAIT(", aID( i ), ") <", iStat, "> ", iMsg
            call zzrc( i + failRCbase )
        end if
    end do

END SUBROUTINE Wait4ID
