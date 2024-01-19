!*  ===================================================================
!*
!*                               Attribute in the ASSOCIATE Construct
!*
!*  DATE                       : February 28, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute
!*  SECONDARY FUNCTIONS TESTED : associate-name => selector (where selector
!*                               implicitly has the ASYNCHRONOUS Attribute)
!*
!*  REQUIRED COMPILER OPTIONS  : -qattr=full
!*
!*  KEYWORD(S)                 : ASYNCHRONOUS Attribute, ASSOCIATE Construct
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  8.1.4.1 Form of the ASSOCIATE construct
!*
!*  R816 associate-construct  is  associate-stmt
!*                                    block
!*                                    end-associate-stmt
!*  R817 associate-stmt       is  [ associate-construct-name : ] ASSOCIATE&
!*                                 &( association-list )
!*  R818 association          is  associate-name => selector
!*  R819 selector             is  expr
!*                            or  variable
!*
!*  8.1.4.3 Attributes of associate names
!*
!*  Within a SELECT TYPE or ASSOCIATE construct, ... The associating entity
!*  has the ASYNCHRONOUS, TARGET, or VOLATILE attribute if and only if the
!*  selector is a variable and has the attribute.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM asynchAttrAssocName02
    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: j
    INTEGER :: iStat

    INTEGER, PARAMETER :: ioUnit = 3

    CHARACTER(LEN = 256) :: iMsg

    CHARACTER(LEN = 1) :: newLineChar
    CHARACTER(LEN = 10), DIMENSION( 39 ) :: iBuffer

    CHARACTER(LEN = 48) :: result
    CHARACTER(LEN = 48) :: expected =&
                    "Hello, --YES-- --unformatted-- --stream-- World!"


    OPEN(ioUnit, FILE='asynchAttrAssocName02.dat',&
        &FORM='formatted', ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    DO i = 1, 39
        j = MOD(i, 2)

        READ(ioUnit, FMT='(a10,a)', ASYNCHRONOUS='yes',&
                    &IOSTAT=iStat, IOMSG=iMsg) iBuffer( i ), newLineChar
        IF (iStat <> 0) THEN
            WRITE(0, *) i, ") READ() <", iStat, "> ", iMsg
            CALL zzrc( 2 )
        END IF
    END DO


    WAIT(ioUnit, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
        CALL zzrc( 3 )
    END IF

    DO i = 39, 1, -1
		PRINT '("iBuffer(",I2,") = <",A10,">")', i, iBuffer( i )
    END DO


    DO i = 1, 39
        j = i + 9

        ASSOCIATE (cVal => iBuffer( i ))
            result( i:j ) = cVal
        END ASSOCIATE
    END DO


    CLOSE(ioUnit, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 4 )
    END IF


    IF (result <> expected) THEN
        WRITE(0, *) "result   = '", result, "'"
        WRITE(0, *) "expected = '", expected, "'"

        CALL zzrc( 5 )
    END IF

END PROGRAM asynchAttrAssocName02
