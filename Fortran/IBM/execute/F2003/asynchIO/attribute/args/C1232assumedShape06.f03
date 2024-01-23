!*  ===================================================================
!*
!*                               Attribute in Array Section Arguments
!*
!*  DATE                       : April 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Actual Argument is an Assumed Shape Array
!*  SECONDARY FUNCTIONS TESTED : Dummy Argument explicitly has the ASYNCHRONOUS
!*                               Attribute, is an Assumed-Size Array, and
!*                               has been passed via a C Function
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASYNCHRONOUS Attribute
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  12.4.1.2 Actual arguments associated with dummy data objects
!*
!*  If the actual argument is an array section having a vector subscript,
!*  the dummy argument is not definable and shall not have the INTENT (OUT),
!*  INTENT (INOUT), VOLATILE, or ASYNCHRONOUS attributes.
!*
!*  C1232 (R1221) If an actual argument is an array section or an
!*                assumed-shape array, and the corresponding dummy argument
!*                has either the VOLATILE or ASYNCHRONOUS attribute, that
!*                dummy argument shall be an assumed-shape array.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program C1232assumedShape06
    USE ISO_C_BINDING

    interface
        integer(C_INT) function CPipeLine(size, buffer) BIND(C)
            USE ISO_C_BINDING
            integer(C_INT) :: size
            real(C_FLOAT), dimension( * ) :: buffer
        end function CPipeLine
    end interface

    integer, parameter :: n = 1000

    integer(C_INT), parameter :: m = 100

    integer(C_INT) :: iStat

    integer, dimension( n ) :: ioID

    real(C_FLOAT), dimension( n ) :: dataIn
    real(C_FLOAT), dimension( n ) :: dataOut

    character(len = 256) :: iMsg


    dataOut = (/ (((REAL( i ) / REAL( n )) + m), i = 1, n) /)


    open(7004, ASYNCHRONOUS='YES', FORM='UNFORMATTED', IOMSG=iMsg,&
                ACTION='READWRITE', ACCESS='SEQUENTIAL', IOSTAT=iStat)
    if (iStat <> 0) then
        write(0, *) "OPEN() <", iStat, "> ", iMsg
        error stop 1
    end if


    do i = 1, n, m
        iStat = CPipeLine(m, dataOut( i:(i + m - 1) ))

        if (iStat <> 0) then
            write(0, *) i, "CPipeLine()"
            error stop 2
        end if
    end do


    rewind(7004, ERR=200)
    goto 300

200 write(0, *) "REWIND() <", iStat, "> ", iMsg
    error stop 3


300 do i = n, 1, -1
        read(7004, ASYNCHRONOUS='YES', ID=ioID( i ),&
                IOSTAT=iStat, IOMSG=iMsg) dataIn( (n - i + 1) )

        if (iStat <> 0) then
            write(0, *) i, ") READ() <", iStat, "> ", iMsg
            error stop 4
        end if
    end do


    do i = 1, n
        wait(7004, ID=ioID( (n - i + 1) ), IOMSG=iMsg, IOSTAT=iStat)
        if (iStat <> 0) then
            write(0, *) i, ") WAIT(", ioID( i ), ") <", iStat, "> ", iMsg
            error stop 5

        else if (dataOut( i ) <> dataIn( i )) then
            write(0, *) " dataIn(", i, ") = '", dataIn( i ), "'"
            write(0, *) "dataOut(", i, ") = '", dataOut( i ), "'"

            error stop 6
        end if
    end do


    close(7004, IOMSG=iMsg, IOSTAT=iStat)
    if (iStat <> 0) then
        write(0, *) "CLOSE() <", iStat, "> ", iMsg
        error stop 7
    end if

end program C1232assumedShape06


integer(C_INT) function writeData(size, buffer) BIND(C)
    USE ISO_C_BINDING

    integer(C_INT) :: size
    real(C_FLOAT), asynchronous, dimension( * ) :: buffer

    integer(C_INT) :: iStat

    character(len = 256) :: iMsg

    iStat = 0
    do i = 1, size
        write(7004, ASYNCHRONOUS='yes',&
            IOSTAT=iStat, IOMSG=iMsg) buffer( i )

        if (iStat <> 0) then
            write(0, *) i, ") WRITE() <", iStat, "> ", iMsg
        end if
    end do

    writeData = iStat

end function writeData
