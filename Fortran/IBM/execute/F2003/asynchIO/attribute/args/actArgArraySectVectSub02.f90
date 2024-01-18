!*  ===================================================================
!*
!*                               Attribute in Array Section Arguments
!*
!*  DATE                       : April  6, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Actual Argument is an Array Section (with a
!*                               Vector Subscript)
!*  SECONDARY FUNCTIONS TESTED : Dummy Argument Specification does *NOT*
!*                               explicitly include the ASYNCHRONOUS Attribute
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
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM actArgArraySectVectSub02

    INTERFACE
        SUBROUTINE DumpSomeData(ioUnit, dataArray)
            INTEGER, INTENT(IN) :: ioUnit
            INTEGER, DIMENSION( 1000 ) :: dataArray
        END SUBROUTINE DumpSomeData
    END INTERFACE

    INTEGER, PARAMETER :: n = 10000
    INTEGER, DIMENSION( n ) :: intArray

    INTEGER, DIMENSION( 1000 ) :: ids

    CHARACTER(LEN = 256) :: iMsg


    OPEN(1225, FORM='unformatted', ASYNCHRONOUS='yes',&
        ACTION='write', ACCESS='stream', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    DO i = 1, n
        intArray( i ) = n - i + 1
    END DO


    CALL DumpSomeData(1225, intArray( intArray( 5000:6000 ) ))


    CLOSE(1225, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 3 )
    END IF

END PROGRAM actArgArraySectVectSub02


SUBROUTINE DumpSomeData(ioUnit, dataArray)
    INTEGER, INTENT(IN) :: ioUnit
    INTEGER, DIMENSION( 1000 ) :: dataArray

    CHARACTER(LEN = 256) :: iMsg


    DO i = 1, 1000
        WRITE(ioUnit, IOSTAT=iStat, IOMSG=iMsg) dataArray( i )
        IF (iStat <> 0) THEN
            WRITE(0, *) i, "WRITE() <", iStat, "> ", iMsg
            CALL zzrc( 2 )
        END IF
    END DO

END SUBROUTINE DumpSomeData
