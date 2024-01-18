!*  ===================================================================
!*
!*                               Attribute with Pointer Array Actual Arguments
!*
!*  DATE                       : April  7, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Actual Argument is a Pointer Array
!*  SECONDARY FUNCTIONS TESTED : Dummy Argument explicitly has the ASYNCHRONOUS
!*                               Attribute, and is a Pointer Array
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

PROGRAM C1233pointerArray02

    character(len = 4) :: tmpbuf
    character(len = 4), pointer, dimension( : ) :: idList

    character(len = 256) :: iMsg


    idListSize = 400

    allocate(idList( idListSize ), STAT=iStat, ERRMSG=iMsg)
    if (iStat <> 0) then
        write(0, *) "ALLOCATE() <", iStat, "> ", iMsg
        call zzrc( 1 )
    end if


    open(1221, FILE='C1233pointerArray02.dat',&
		FORM='unformatted', ASYNCHRONOUS='yes', RECL=4,&
        ACTION='read', ACCESS='direct', IOSTAT=iStat, IOMSG=iMsg)
    if (iStat <> 0) then
        write(0, *) "OPEN() <", iStat, "> ", iMsg
        call zzrc( 2 )
    end if


    call LoadData(1221, idListSize, idList)


    close(1221, IOSTAT=iStat, IOMSG=iMsg)
    if (iStat <> 0) then
        write(0, *) "CLOSE() <", iStat, "> ", iMsg
        call zzrc( 4 )
    end if


    do i = 1, idListSize, 8
        write(6, '(8A5)') (idList( j ), j = i, (i + 7))
    end do


    CONTAINS

        SUBROUTINE LoadData(ioUnit, size, dataArray)
            integer :: ioUnit
            integer :: size
            character(len = 4), pointer,&
                asynchronous, dimension( : ) :: dataArray

            character(len = 256) :: iMsg


            do i = 1, size
                read(ioUnit, asynchronous='yes', REC=i,&
                    iostat=iStat, iomsg=iMsg) dataArray( i )

                if (iStat <> 0) then
                    write(0, *) i, "READ() <", iStat, "> ", iMsg
                    call zzrc( 3 )
                end if
            end do

        END SUBROUTINE LoadData

END PROGRAM C1233pointerArray02
