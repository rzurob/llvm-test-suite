!*  ===================================================================
!*
!*                               Attribute with Pointer Array Actual Arguments
!*
!*  DATE                       : April 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Actual Argument is a Pointer Array
!*  SECONDARY FUNCTIONS TESTED : Dummy Argument explicitly has both the
!*                               ASYNCHRONOUS and VOLATILE Attributes, and
!*                               is an Assumed-Shape Array
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASYNCHRONOUS Attribute, POINTER Attribute,
!*                               TARGET Attribute, VOLATILE Attribute
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
!*  C1233 (R1221) If an actual argument is a pointer array, and the
!*        corresponding dummy argument has either the VOLATILE or ASYNCHRONOUS
!*        attribute, that dummy argument shall be an assumed-shape array or
!*        a pointer array.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mModule
    contains

        subroutine ReportWriter(theUnit, n, idx, someData)
            integer, intent(in) :: theUnit
            integer, intent(in) :: n
            integer, intent(in) :: idx
            complex, dimension( 0: ) :: someData

            character(len = 256) :: iMsg


            do i = 1, n
                write(theUnit, asynchronous='yes', iostat=iStat,&
                            rec=(idx + i), iomsg=iMsg) someData( i )

                if (iStat <> 0) then
                    write(0, *) "WRITE() <", iStat, "> ", iMsg
                    call zzrc( 2 )
                end if
            end do

        end subroutine ReportWriter


        subroutine ReportReader(theUnit, n, idx, someData)
            integer, intent(in) :: theUnit
            integer, intent(in) :: n
            integer, intent(in) :: idx
            complex, dimension( 0: ) :: someData

            character(len = 256) :: iMsg


            do i = 0, n
                read(theUnit, asynchronous='yes', iostat=iStat,&
                            rec=(idx + i), iomsg=iMsg) someData( i )

                if (iStat <> 0) then
                    write(0, *) "READ() <", iStat, "> ", iMsg
                    call zzrc( 4 )
                end if
            end do

        end subroutine ReportReader

end module mModule

PROGRAM C1233pointerArray07
    use mModule

    integer, parameter :: m = 100
    integer, parameter :: n = 1000

    complex, target, dimension( m,n ) :: theData
    complex, pointer, dimension( : ) :: dataPtr

    character(len = 256) :: iMsg


    do i = 1, n
        do j = 1, m
            theData( j,i ) =&
                CMPLX( (REAL( j ) / REAL( m )),(REAL( i ) / REAL( n )) )
        end do
    end do


    open(3321, asynchronous='yes', access='direct', recl=8,&
        form='unformatted', action='readwrite', iostat=iStat, ioMsg=iMsg)
    if (iStat <> 0) then
        write(0, *) "OPEN() <", iStat, "> ", iMsg
        call zzrc( 1 )
    end if


    dataPtr => theData( :,1 )
    call ReportWriter(3321, m, 1, dataPtr)

    do i = 2, n
        dataPtr => theData( :,i )
        call ReportWriter(3321, m, (i * m + 1), dataPtr)

        WAIT(3321, iostat=iStat, iomsg=iMsg)
        if (iStat <> 0) then
            write(0, *) "CLOSE() <", iStat, "> ", iMsg
            call zzrc( 3 )
        end if

        j = i - 1
        dataPtr => theData( :,j )
        call ReportReader(3321, 100, (j * m + 1), dataPtr)
    end do

    dataPtr => theData( :,n )
    call ReportReader(3321, 100, (n * m + 1), dataPtr)


    close(3321, iostat=iStat, ioMsg=iMsg)
    if (iStat <> 0) then
        write(0, *) "CLOSE() <", iStat, "> ", iMsg
        call zzrc( 1 )
    end if

END PROGRAM C1233pointerArray07
