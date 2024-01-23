!*  ===================================================================
!*
!*                               Attribute in Array Section Arguments
!*
!*  DATE                       : April  7, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Actual Argument is an Array Section
!*  SECONDARY FUNCTIONS TESTED : Dummy Argument explicitly has the ASYNCHRONOUS
!*                               and VOLATILE Attributes, and is an Assumed-
!*                               Shape Array
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

program C1232arraySect05

    INTERFACE
        SUBROUTINE SumDump(ioUnit, n, asynchData)
            integer, intent(IN) :: ioUnit
            integer, intent(IN) :: n
            real, VOLATILE, ASYNCHRONOUS, DIMENSION( : ) :: asynchData
        END SUBROUTINE SumDump
    END INTERFACE


    INTEGER, PARAMETER :: n = 10000

    REAL, DIMENSION( n ) :: sumData = (/ ((i * 5280.0), i = 1, n) /)

    CHARACTER(LEN = 256) :: iMsg


    sumData = sumData * 12.0


    OPEN(4, ASYNCHRONOUS='yes', FORM='unformatted', IOMSG=iMsg,&
                ACTION='readwrite', ACCESS='stream', IOSTAT=iStat)
    if (iStat <> 0) then
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        error stop 1
    end if


    DO i = 1, (n / 100)
        j = (i - 1) * 100 + 1
        l = j + 99

        sumData( j:l ) = sumData( j:l ) * 2.54

        call SumDump(4, i, sumData( j:l ))
    END DO


    rewind 4


    do i = 1, n
        READ(4, IOSTAT=iStat, IOMSG=iMsg) realValue

        if (iStat <> 0) then
            WRITE(0, *) i, ") WRITE() <", iStat, "> ", iMsg
            ERROR STOP 3

        end if

        WRITE(6, '(I5,F12.5,F12.5)') i, realValue, sumData( i )

        if (realValue <> sumData( i )) then
            ERROR STOP 4
        end if
    end do

    CLOSE(4, IOMSG=iMsg, IOSTAT=iStat)
    if (iStat <> 0) then
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        error stop 5
    end if

end program C1232arraySect05


SUBROUTINE SumDump(ioUnit, n, asynchData)
    integer, intent(IN) :: ioUnit
    integer, intent(IN) :: n
    real, VOLATILE, ASYNCHRONOUS, DIMENSION( : ) :: asynchData

    CHARACTER(len = 256) :: iMsg


    asynchData = asynchData / 100


    DO i = 1, 100
        asynchData( i ) = asynchData( i ) / 1000

        WRITE(ioUnit, ASYNCHRONOUS='yes',&
            IOSTAT=iStat, IOMSG=iMsg) asynchData( i )

        if (iStat <> 0) then
            WRITE(0, *) n, ",", i, ") WRITE() <", iStat, "> ", iMsg
            ERROR STOP 2
        end if
    END DO

END SUBROUTINE SumDump
