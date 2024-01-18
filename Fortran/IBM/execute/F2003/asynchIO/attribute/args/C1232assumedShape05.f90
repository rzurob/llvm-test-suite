!*  ===================================================================
!*
!*                               Attribute in Assumed-Shape Array Arguments
!*
!*  DATE                       : April  7, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Actual Argument is an Assumed-Shape Array
!*  SECONDARY FUNCTIONS TESTED : Dummy Argument explicitly has the ASYNCHRONOUS
!*                               and VOLATILE Attributes, and is an
!*                               Assumed-Shape Array
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

MODULE mAysnchVolatile

    CONTAINS

        SUBROUTINE VolatileAsynch(ioUnit, size, aVolArray, iStat)
            INTEGER :: ioUnit
            INTEGER :: size
            COMPLEX, ASYNCHRONOUS, VOLATILE, DIMENSION( : ) :: aVolArray
            INTEGER :: iStat

            CHARACTER(len = 256) :: iMsg


            i = 0
            iStat = 0

            DO WHILE ((i < size)  .AND.  (iStat == 0))
                i = i + 1
                WRITE(ioUnit, ASYNCHRONOUS='yes',&
                    IOSTAT=iStat, IOMSG=iMsg) aVolArray( i )
                IF (iStat <> 0) THEN
                    WRITE(0, *) i, " WRITE(ASYNCHRONOUS=yes) <",&
                                                    iStat, "> ", iMsg
                END IF
            END DO

        END SUBROUTINE VolatileAsynch

END MODULE mAysnchVolatile

MODULE mAssumeShape
    USE mAysnchVolatile

    CONTAINS

        SUBROUTINE ShapeIsAssumed(ioUnit, size, arrayAssumeShape, iStat)
            INTEGER :: ioUnit
            INTEGER :: size
            COMPLEX, DIMENSION( : ) :: arrayAssumeShape
            INTEGER :: iStat


            DO i = 1, size
                arrayAssumeShape( i ) = CMPLX( (size + i),(size - i) )
            END DO

            CALL VolatileAsynch(ioUnit, size, arrayAssumeShape, iStat)

        END SUBROUTINE ShapeIsAssumed
END MODULE mAssumeShape


PROGRAM C1232assumedShape05
    USE mAssumeShape

    INTEGER, PARAMETER :: n = 250

    COMPLEX :: testValue
    COMPLEX, DIMENSION( n ) :: complexArray

    CHARACTER(len = 256) :: iMsg


    OPEN(96, ACTION='readwrite', ACCESS='stream', IOMSG=iMsg,&
            ASYNCHRONOUS='yes', FORM='unformatted', IOSTAT=iStat)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    CALL ShapeIsAssumed(96, n, complexArray, iStat)
    IF (iStat <> 0) THEN
        CALL zzrc( 2 )
    END IF


    REWIND(96, IOMSG=iMsg, IOSTAT=iStat)
    IF (iStat <> 0) THEN
        WRITE(0, *) "REWIND() <", iStat, "> ", iMsg
        CALL zzrc( 3 )
    END IF


    DO i = 1, n
        READ(96, IOSTAT=iStat, IOMSG=iMsg) testValue
        IF (iStat <> 0) THEN
            WRITE(0, *) i, "READ(ASYNCHRONOUS=no) <", iStat, "> ", iMsg
            CALL zzrc( 4 )
        END IF

        IF (complexArray( i ) <> testValue) THEN
            WRITE(0, *) "complexArray(", i, ") =",&
                        complexArray( i ), ", testValue =", testValue
            CALL zzrc( 5 )
        END IF
    END DO


    CLOSE(96, IOMSG=iMsg, IOSTAT=iStat)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 6 )
    END IF

END PROGRAM C1232assumedShape05
