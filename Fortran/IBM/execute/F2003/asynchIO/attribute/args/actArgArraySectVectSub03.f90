!*  ===================================================================
!*
!*                               Attribute in Array Section Arguments
!*
!*  DATE                       : April 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Actual Argument is an Array Section (with a
!*                               Vector Subscript)
!*  SECONDARY FUNCTIONS TESTED : Dummy Argument Specification is for a C
!*                               function and does *NOT* explicitly include
!*                               the ASYNCHRONOUS Attribute
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

PROGRAM actArgArraySectVectSub03
    USE ISO_C_BINDING

    INTERFACE
        INTEGER(C_INT) FUNCTION COpen( ) BIND(C)
            USE ISO_C_BINDING
        END FUNCTION COpen

        INTEGER(C_INT) FUNCTION CWriteData(size, dataArray) BIND(C)
            USE ISO_C_BINDING
            INTEGER(C_INT) :: size
            REAL(C_FLOAT), DIMENSION( * ) :: dataArray
        END FUNCTION CWriteData

        INTEGER(C_INT) FUNCTION CClose( ) BIND(C)
            USE ISO_C_BINDING
        END FUNCTION CClose
    END INTERFACE


    INTEGER, PARAMETER :: n = 1000

    INTEGER(C_INT) :: iStat

    REAL(C_FLOAT), DIMENSION( n ) :: dataIn
    REAL(C_FLOAT), DIMENSION( n ) :: dataOut =&
                    (/ ((REAL( i ) / REAL( n )), i = 1, n) /)

    CHARACTER(LEN = 256) :: iMsg


    iStat = COpen( )
    IF (iStat <> 0) THEN
        CALL zzrc( 1 )
    END IF

    DO i = 1, 10
        iStat = CWriteData((n / 10), dataOut( (/ (j, j = i, n, 10) /) ))
        IF (iStat <> 0) THEN
            CALL zzrc( 2 )
        END IF
    END DO

    iStat = CClose( )
    IF (iStat <> 0) THEN
        CALL zzrc( 3 )
    END IF


    OPEN(4002, FILE='actArgArraySectVectSub03.dat',&
        ACTION='read', ACCESS='stream', IOMSG=iMsg,&
        ASYNCHRONOUS='yes', FORM='unformatted', IOSTAT=iStat)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 4 )
    END IF


    DO i = 1, 10
        READ(4002, ASYNCHRONOUS='yes', IOSTAT=iStat,&
                IOMSG=iMsg) (dataIn( j ), j = i, n, 10)

        IF (iStat <> 0) THEN
            WRITE(0, *) i, ") READ() <", iStat, "> ", iMsg
            CALL zzrc( 5 )
        END IF
    END DO


    CLOSE(4002, IOMSG=iMsg, IOSTAT=iStat)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 6 )
    END IF


    DO i = 1, n
        IF (dataOut( i ) <> dataIn( i )) THEN
            WRITE(0, *) " dataIn(", i, ") = '", dataIn( i ), "'"
            WRITE(0, *) "dataOut(", i, ") = '", dataOut( i ), "'"

            CALL zzrc( 7 )
        END IF
    END DO

END PROGRAM actArgArraySectVectSub03
