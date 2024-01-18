!*  ===================================================================
!*
!*                               Attribute in Array Section Arguments
!*
!*  DATE                       : April 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Actual Argument is an Array Section
!*  SECONDARY FUNCTIONS TESTED : Dummy Argument explicitly has the ASYNCHRONOUS
!*                               Attribute, is an Assumed-Shape Array, and
!*                               is passed from a C Function
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASYNCHRONOUS Attribute, VOLATILE Attribute
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

program C1232arraySect06
    USE iso_c_binding

    INTERFACE
        integer(C_INT) function CSectionReads(size, buffer) BIND(C)
            USE iso_c_binding
            integer(C_INT) :: size
            real(C_FLOAT), dimension( * ) :: buffer
        end function CSectionReads
    END INTERFACE

    integer(C_INT) :: iStat
    integer(C_INT), parameter :: n = 1000

    real(C_FLOAT), dimension( n ) :: dataIn
    real(C_FLOAT), dimension( n ) :: dataOut =&
            (/ ((REAL( i ) / REAL( n )), i = 1, n) /)

    character(len = 256) :: iMsg


    open(403, asynchronous='yes', action='readwrite',&
        form='unformatted', access='stream', iostat =iStat, iomsg=iMsg)
    if (iStat <> 0) then
        write(0, *) "OPEN() <", iStat, "> ", iMsg
        call zzrc( 1 )
    end if


    do i = 1, n
        write(403, ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=iMsg) dataOut( i )
        if (iStat <> 0) then
            write(0, *) i, ") WRITE() <", iStat, "> ", iMsg
            call zzrc( 2 )
        end if
    end do


    rewind(403, iostat =iStat, iomsg=iMsg)
    if (iStat <> 0) then
        write(0, *) "REWIND() <", iStat, "> ", iMsg
        call zzrc( 3 )
    end if


    iStat = CSectionReads(n, dataIn)
    if (iStat <> 0) then
        call zzrc( 4 )
    end if


    do i = 1, n
        if (dataOut( i ) <> dataIn( i )) then
            write(0, *) " dataIn(", i, ") = '", dataIn( i ), "'"
            write(0, *) "dataOut(", i, ") = '", dataOut( i ), "'"

            call zzrc( 5 )
        end if
    end do


    close(403, iostat =iStat, iomsg=iMsg)
    if (iStat <> 0) then
        write(0, *) "CLOSE() <", iStat, "> ", iMsg
        call zzrc( 6 )
    end if

end program C1232arraySect06


integer(C_INT) function readData(size, buffer)
    USE iso_c_binding

    integer(C_INT) :: size
    real(C_FLOAT), asynchronous, dimension( * ) :: buffer

    integer(C_INT) :: iStat

    character(len = 256) :: iMsg


    i = 0
    iStat = 0

    do while ((i < size)  .AND.  (iStat == 0))
        i = i + 1

        read(403, asynchronous='yes', iostat=iStat, iomsg=iMsg) buffer( i )
        if (iStat <> 0) then
            write(0, *) i, ") READ() <", iStat, "> ", iMsg
        end if
    end do


    readData = iStat

end function readData
