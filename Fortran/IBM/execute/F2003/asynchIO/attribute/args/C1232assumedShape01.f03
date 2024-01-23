!*  ===================================================================
!*
!*                               Attribute in Assumed-Shape Array Arguments
!*
!*  DATE                       : April  7, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Actual Argument is an Assumed-Shape Array
!*  SECONDARY FUNCTIONS TESTED : Dummy Argument explicitly has the ASYNCHRONOUS
!*                               Attribute, and is an Assumed-Shape Array
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

PROGRAM C1232assumedShape01

    character(len = 8) :: tmpBuf
    character(len = 8), dimension( 1000 ) :: dataArray

    character(len = 256) :: iMsg


    OPEN(1232, FILE='C1232assumedShape01.dat',&
        ACTION='read', ACCESS='stream', IOMSG=iMsg,&
        ASYNCHRONOUS='yes', FORM='unformatted', IOSTAT=iStat)
    if (0 <> iStat) then
        write(0, *) "OPEN() <", iStat, "> ", iMsg
        error stop 1
    end if


    call ProcessFile(1232, dataArray)


    CLOSE(1232, IOMSG=iMsg, IOSTAT=iStat)
    if (0 <> iStat) then
        write(0, *) "CLOSE() <", iStat, "> ", iMsg
        error stop 4
    end if


    CONTAINS

        SUBROUTINE ProcessFile(ioUnit, theArray)
            INTEGER, INTENT(in) :: ioUnit
            character(len = 8), dimension( 0: ) :: theArray

            iStat = ReadData(ioUnit, theArray)
            if (iStat <> 0) then
                error stop 2
            end if


            WAIT(ioUnit, IOMSG=iMsg, IOSTAT=iStat)
            if (0 <> iStat) then
                write(0, *) "WAIT() <", iStat, "> ", iMsg
                error stop 3
            end if


            do i = 0, 999, 4
                write(6, '(4A9)') (theArray( j ), j = i, (i + 3))
            end do

        END SUBROUTINE ProcessFile


        INTEGER FUNCTION ReadData(ioUnit, anArray)
            INTEGER, INTENT(in) :: ioUnit
            CHARACTER(len = 8), ASYNCHRONOUS, DIMENSION( -500: ) :: anArray

            CHARACTER(len = 256) :: iMsg


            i = -501
            do while ((i < 500)  .AND.  (iStat == 0))
                i = i + 1

                read(ioUnit, ASYNCHRONOUS='yes',&
                    IOSTAT=iStat, IOMSG=iMsg) anArray( i )

                if (iStat <> 0) then
                    write(0, *) i, "READ() <", iStat, "> ", iMsg
                end if
            end do

            ReadData = iStat

        END FUNCTION ReadData

END PROGRAM C1232assumedShape01
