!*  ===================================================================
!*
!*                               Attribute with Pointer Array Actual Arguments
!*
!*  DATE                       : April  7, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Actual Argument is a Pointer Array
!*  SECONDARY FUNCTIONS TESTED : Dummy Argument explicitly has the ASYNCHRONOUS
!*                               Attribute, and is an Assumed-Shape Array
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

PROGRAM C1233pointerArray01

    INTERFACE
        INTEGER FUNCTION SaveData(ioUnit, size, theData)
            INTEGER, INTENT(IN) :: ioUnit
            INTEGER, INTENT(IN) :: size
            REAL, ASYNCHRONOUS, DIMENSION( 0: ) :: theData
        END FUNCTION SaveData
    END INTERFACE

    INTEGER, PARAMETER :: n = 250

    REAL :: readValue

    REAL, POINTER, DIMENSION( : ) :: arrayPtr

    REAL, TARGET, DIMENSION( n ) :: dataArray = (/ (i, i = 1, n) /)

    CHARACTER(LEN = 256) :: iMsg


    OPEN(1233, FORM='unformatted', ACTION='readwrite',&
        ACCESS='stream', ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    arrayPtr => dataArray

    iStat = SaveData(1233, n, arrayPtr)
    IF (iStat <> 0) THEN
        CALL zzrc( 2 )
    END IF


    REWIND(1233, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "REWIND() <", iStat, "> ", iMsg
        CALL zzrc( 3 )
    END IF


    DO i = 1, n
        READ(1233, IOSTAT=iStat, IOMSG=iMsg) readValue
        IF (iStat <> 0) THEN
            WRITE(0, *) i, "READ() <", iStat, "> ", iMsg
            CALL zzrc( 4 )

        ELSE IF (readValue <> dataArray( i )) THEN
            WRITE(0, *) "dataArray(", i, ") =",&
                        dataArray( i ), ", readValue =", readValue
            CALL zzrc( 5 )
        END IF
    END DO


    CLOSE(1233, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 6 )
    END IF

END PROGRAM C1233pointerArray01


INTEGER FUNCTION SaveData(ioUnit, size, theData)
    INTEGER, INTENT(IN) :: ioUnit
    INTEGER, INTENT(IN) :: size
    REAL, ASYNCHRONOUS, DIMENSION( 0: ) :: theData

    INTEGER :: iStat = 0

    CHARACTER(LEN = 256) :: iMsg = ''


    i = 0
    DO WHILE ((i < size)  .AND.  (iStat == 0))
        WRITE(ioUnit, ASYNCHRONOUS='yes',&
            IOSTAT=iStat, IOMSG=iMsg) theData( i )

        IF (iStat <> 0) THEN
            WRITE(0, *) i, "WRITE() <", iStat, "> ", iMsg
        END IF

        i = i + 1
    END DO


    SaveData = iStat

END FUNCTION SaveData
