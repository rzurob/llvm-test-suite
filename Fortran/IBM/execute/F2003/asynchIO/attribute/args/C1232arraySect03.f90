!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : C1232arraySect03 - ASYNCHRONOUS
!*                               Attribute in Array Section Arguments
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : April  6, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Actual Argument is an Array Section
!*  SECONDARY FUNCTIONS TESTED : Dummy Argument implicitly has the ASYNCHRONOUS
!*                               Attribute and is an Assumed-Shape Array
!*
!*  DRIVER STANZA              : xlf2003
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

module asynchModule

    contains
        subroutine asynchSub(asynchUnit, n, asynchData)
            integer, intent(in) :: asynchUnit
            integer, intent(in) :: n
            integer, dimension( : ) :: asynchData

            character(len = 256) :: iMsg


            do i = 1, n
                write(asynchUnit, asynchronous='yes',&
                        iostat=iStat, iMsg=iMsg) asynchData( i )

                if (0 <> iStat) then
                    write(0, *) i, "WRITE() <", iStat, "> ", iMsg
                    call zzrc( 2 )
                end if
            end do

        end subroutine asynchSub

end module asynchModule

program C1232arraySect03
    use asynchModule

    integer, dimension( 1000 ) :: someData = (/ ((i + 29), i = 1, 1000) /)

    character(len = 256) :: iMsg


    open(1232, ACTION='write', ACCESS='sequential', IOMSG=iMsg,&
            ASYNCHRONOUS='yes', FORM='unformatted', IOSTAT=iStat)
    if (0 <> iStat) then
        write(0, *) "OPEN() <", iStat, "> ", iMsg
        call zzrc( 1 )
    end if


    do i = 0, 99
        call asynchSub(1232, 10, someData( ((i * 10) + 1):((i * 10) + 100)) )
    end do


    wait(1232, IOMSG=iMsg, IOSTAT=iStat)
    if (0 <> iStat) then
        write(0, *) "WAIT() <", iStat, "> ", iMsg
        call zzrc( 3 )
    end if


    close(1232, IOMSG=iMsg, IOSTAT=iStat)
    if (0 <> iStat) then
        write(0, *) "CLOSE() <", iStat, "> ", iMsg
        call zzrc( 4 )
    end if

end program C1232arraySect03
