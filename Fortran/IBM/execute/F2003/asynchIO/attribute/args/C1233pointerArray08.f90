!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : C1233pointerArray08 - ASYNCHRONOUS
!*                               Attribute with Pointer Array Actual Arguments
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : April 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Actual Argument is a Pointer Array
!*  SECONDARY FUNCTIONS TESTED : Dummy Argument explicitly has both the
!*                               ASYNCHRONOUS and VOLATILE Attributes, and
!*                               is a Pointer Array
!*
!*  DRIVER STANZA              : xlf2003
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

PROGRAM C1233pointerArray08

    INTERFACE
        INTEGER FUNCTION DumpData(aUnit, size, aIDs, aData)
            integer, intent(IN) :: aUnit
            integer, intent(IN) :: size
            integer, dimension( : ), intent(IN) :: aIDs
            integer, asynchronous, volatile, pointer, dimension( : ) :: aData
        END FUNCTION DumpData
    END INTERFACE

    integer, parameter :: n = 10000

    integer :: dStat
    integer, dimension( n ) :: ioIDs

    integer, pointer, dimension( : ) :: theData

    character(len = 256) :: iMsg


    allocate(theData( n ), stat=iStat, errmsg=iMsg)
    if (iStat <> 0) then
        write(0, *) "ALLOCATE() <", iStat, "> ", iMsg
        call zzrc( 1 )
    end if


    open(304007, file='C1233pointerArray08.dat',&
        action='read', iomsg=iMsg, access='stream',&
        asynchronous='yes', form='unformatted', iostat=iStat)
    if (iStat <> 0) then
        write(0, *) "OPEN() <", iStat, "> ", iMsg
        call zzrc( 2 )
    end if


    i = 0
    j = n / 10

    iStat = 0
    do while ((i < j)  .AND. (iStat == 0))
        k = i * 10
        i = i + 1

        READ(304007, asynchronous='yes', ID=ioIDs( i ),&
            iomsg=iMsg, iostat=iStat) (theData( l ), l = (k + 1), (k + 10))

        if (iStat <> 0) then
            write(0, *) i, ") READ() <", iStat, "> ", iMsg
            call zzrc( 3 )
        end if
    end do


    dStat = DumpData(304007, n, ioIDs, theData)


    close(304007, iomsg=iMsg, iostat=iStat)
    if (dStat <> 0) then
        call zzrc( 4 )

    else if (iStat <> 0) then
        write(0, *) "CLOSE() <", iStat, "> ", iMsg
        call zzrc( 5 )
    end if

END PROGRAM C1233pointerArray08


INTEGER FUNCTION DumpData(aUnit, size, aIDs, aData)
    integer, intent(IN) :: aUnit
    integer, intent(IN) :: size
    integer, dimension( : ), intent(IN) :: aIDs
    integer, asynchronous, volatile, pointer, dimension( : ) :: aData

    character(len = 256) :: iMsg


    i = 0
    iStat = 0

    do while ((i < (size / 10))  .AND.  (iStat == 0))
        i = i + 1

        WAIT(aUnit, ID=aIDs( i ), IOSTAT=iStat, IOMSG=iMsg)
        IF (iStat <> 0) then
            write(0, *) i, "WAIT(", aIDs( i ), ") <", iStat, "> ", iMsg
        END IF
    end do


    if (iStat == 0) then
        do i = 1, size, 10
            write(6, '(10I8)') (aData( j ), j = i, (i + 9))
        end do
    END IF


    DumpData = iStat

END FUNCTION DumpData
