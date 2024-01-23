!*  ===================================================================
!*
!*                               Attribute with Pointer Array Actual Arguments
!*
!*  DATE                       : April  7, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Actual Argument is a Pointer Array
!*  SECONDARY FUNCTIONS TESTED : Dummy Argument implicitly has the ASYNCHRONOUS
!*                               Attribute, and is an Assumed-Shape
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASYNCHRONOUS Attribute, POINTER Attribute,
!*                               TARGET Attribute
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

module mDataAccess
    contains

        integer function DataGet(ioUnit, dataArray)
            integer, intent(in) :: ioUnit
            logical, dimension( 0: ) :: dataArray

            character(len = 256) :: iMsg = ''


            i = 0
            iStat = 0

            do while ((i < 10)  .AND.  (iStat == 0))
                j = i * 10

                read(ioUnit, asynchronous='yes', iostat=iStat,&
                        iomsg=iMsg) (dataArray( k ), k = j, (j + 9))
                if (iStat <> 0) then
                    write(0, *) i, "READ() <", iStat, "> ", iMsg
                end if

                i = i + 1
            end do

            DataGet = iStat

        end function DataGet

end module mDataAccess

program C1233pointerArray04
    use mDataAccess

    logical, dimension( 1000 ) :: logVal
    logical, target, dimension( 1000 ) :: logicalArray
    logical, pointer, dimension( : ) :: ptrLogArray

    character(len = 256) :: iMsg


    open(1233, FILE='C1233pointerArray04.dat',&
        FORM='unformatted', ACTION='read', IOMSG=iMsg,&
        ACCESS='sequential', ASYNCHRONOUS='yes', IOSTAT=iStat)
    if (iStat <> 0) then
        write(0, *) "OPEN() <", iStat, "> ", iMsg
        error stop 1
    end if


    do i = 0, 9
        j = i * 100
        ptrLogArray => logicalArray( (j + 1):(j + 100) )

        iStat = DataGet(1233, ptrLogArray)
        if (iStat <> 0) then
            call zzrc( (10 + i) )
        end if
    end do


    wait(1233, IOMSG=iMsg, IOSTAT=iStat)
    if (iStat <> 0) then
        write(0, *) "WAIT() <", iStat, "> ", iMsg
        error stop 21
    end if


    do i = 1, 1000, 25
        write(6, '(25L2)') (logicalArray( j ), j = i, (i + 24))
    end do


    close(1233, IOMSG=iMsg, IOSTAT=iStat)
    if (iStat <> 0) then
        write(0, *) "CLOSE() <", iStat, "> ", iMsg
        error stop 31
    end if

end program C1233pointerArray04
