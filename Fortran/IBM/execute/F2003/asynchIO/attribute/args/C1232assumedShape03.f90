!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : C1232assumedShape03 - ASYNCHRONOUS
!*                               Attribute in Assumed-Shape Array Arguments
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : April  7, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Actual Argument is an Assumed-Shape Array
!*  SECONDARY FUNCTIONS TESTED : Dummy Argument implicitly has the ASYNCHRONOUS
!*                               Attribute, and is an Assumed-Shape Array
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

INTEGER FUNCTION GrabData(ioUnit, size, inData)
    integer :: ioUnit
    integer :: size
    real, dimension( 0: ) :: inData

    character(len = 256) :: iMsg = ''

    i = 0
    iStat = 0
    do while ((i < size)  .AND.  (iStat == 0))
        read(ioUnit, asynchronous='yes',&
            rec=(i + 1), iostat=iStat, iomsg=iMsg) inData( i )

        i = i + 1
    end do


    if (iStat <> 0) then
        WRITE(0, *) i, "READ() <", iStat, "> ", iMsg
    end if


    GrabData = iStat

END FUNCTION GrabData


INTEGER FUNCTION CheckData(ioUnit, size, newData, oldData)

    integer :: ioUnit
    integer :: size
    real, dimension( : ) :: newData
    real, dimension( : ) :: oldData

    INTERFACE
        INTEGER FUNCTION GrabData(ioUnit, size, inData)
            integer :: ioUnit
            integer :: size
            real, dimension( 0: ) :: inData
        END FUNCTION GrabData
    END INTERFACE

    character(len = 256) :: iMsg = ''


    iStat = GrabData(ioUnit, size, oldData)
    if (iStat == 0) then
        WAIT(ioUnit, IOSTAT=iStat, IOMSG=iMsg)

        if (iStat <> 0) then
            WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
        end if
    end if


    if (iStat == 0) then
        iStat = 0

        do i = 1, size
            if (newData( i ) <> oldData( i )) then
                write(0, *) "newData(", i, ") = '", newData( i ), "'",&
                            "oldData(", i, ") = '", oldData( i ), "'"

                iStat = iStat + 1
            end if
        end do
    end if


    CheckData = iStat

END FUNCTION CheckData


PROGRAM C1232assumedShape03

    INTERFACE
        INTEGER FUNCTION CheckData(ioUnit, size, newData, oldData)
            integer :: ioUnit
            integer :: size
            real, dimension( : ) :: newData
            real, dimension( : ) :: oldData
        END FUNCTION CheckData
    END INTERFACE

    integer, parameter :: n = 250

    real, dimension( n ) :: oldDataBuffer

    real, dimension( n ) :: newData = (/ (i, i = 1, n) /)

    character(len = 256) :: iMsg = ''


    OPEN(1232, ASYNCHRONOUS='yes', ACTION='readwrite', IOMSG=iMsg,&
            ACCESS='direct', FORM='unformatted', RECL=4, IOSTAT=iStat)
    if (iStat <> 0) then
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        call zzrc( 1 )
    end if


    do i = 1, n
        WRITE(1232, REC=i, ASYNCHRONOUS='no',&
            IOMSG=iMsg, IOSTAT=iStat) newData( i )

        if (iStat <> 0) then
            WRITE(0, *) i, "WRITE() <", iStat, "> ", iMsg
            call zzrc( 2 )
        end if
    end do


    iStat = CheckData(1232, n, newData, oldDataBuffer)
    if (iStat <> 0) then
        write(0, *) "C1232assumedShape03()  iStat =", iStat
        call zzrc( 3 )
    end if


    CLOSE(1232, IOMSG=iMsg, IOSTAT=iStat)
    if (iStat <> 0) then
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        call zzrc( 4 )
    end if

END PROGRAM C1232assumedShape03
