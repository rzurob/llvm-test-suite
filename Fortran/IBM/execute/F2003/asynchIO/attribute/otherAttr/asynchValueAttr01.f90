!*  ===================================================================
!*
!*                               Interactions with Other Attributes
!*
!*  DATE                       : February 17, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute
!*  SECONDARY FUNCTIONS TESTED : Interactions with the VALUE Attribute
!*
!*  REQUIRED COMPILER OPTIONS  : -qattr=full
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  5.1 Type declaration statements
!*
!*  R501 type-declaration-stmt  is  declaration-type-spec [ [ , attr-spec ]&
!*                                      &... :: ] entity-decl-list
!*
!*  R502 declaration-type-spec  is  intrinsic-type-spec
!*
!*  R503 attr-spec  is  access-spec
!*                  or  ALLOCATABLE
!*                  or  ASYNCHRONOUS
!*  ...
!*                  or  VALUE
!*
!*  5.1.2.15 VALUE attribute
!*
!*  The VALUE attribute specifies a type of argument association (12.4.1.2)
!*  for a dummy argument.
!*
!*  5.2.14 VALUE statement
!*
!*  R547 value-stmt  is  VALUE [ :: ] dummy-arg-name-list
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mPoint
    IMPLICIT NONE

    TYPE tPoint
        INTEGER :: x
        INTEGER :: y
        INTEGER :: z
    END TYPE tPoint
END MODULE mPoint


PROGRAM asynchValueAttr01
    USE mPoint

    IMPLICIT NONE

    INTERFACE
        INTEGER FUNCTION SavePoint(ioUnit, block, sPt)
            USE mPoint

            IMPLICIT NONE

            INTEGER, INTENT(IN) :: ioUnit
            LOGICAL, INTENT(IN) :: block
            TYPE(tPoint), VALUE, ASYNCHRONOUS :: sPt
        END FUNCTION SavePoint
    END INTERFACE

    INTEGER oStat
    INTEGER wStat
    INTEGER :: oUnit = 751
    CHARACTER(LEN = 256) oMsg

    INTEGER :: i
    INTEGER :: j
    LOGICAL :: block
    REAL, DIMENSION( 3 ) :: randomPt
    TYPE(tPoint), DIMENSION( 1000 ) :: ptList
    TYPE(tPoint), DIMENSION( 1000 ) :: ptCheck


    DO i = 1, 1000
        CALL RANDOM_NUMBER( randomPt )

        ptList( i )%x = INT(randomPt( 1 ) * i)
        ptList( i )%y = INT(randomPt( 2 ) * (-i) + i / 2)
        ptList( i )%z = INT(randomPt( 3 ) * i - i)
    END DO


    OPEN(UNIT=oUnit, FILE='asynchValueAttr01.dat',&
        &FORM='formatted', ASYNCHRONOUS='yes', ACTION='readwrite',&
                    &ACCESS='sequential', IOSTAT=oStat, IOMSG=oMsg)
    IF (oStat <> 0) THEN
        WRITE(0, *) 'asynchValueAttr01()  OPEN() <', oStat, '> ', oMsg
        CALL zzrc( 1 )
    END IF


    block = .TRUE.
    DO i = 1, 1000
        wStat = SavePoint(oUnit, block, ptList( i ))
        block = .NOT. block
    END DO


    WAIT(oUnit, IOSTAT=oStat, IOMSG=oMsg)
    IF (oStat <> 0) THEN
        WRITE(0, *) 'asynchValueAttr01()  WAIT() <', oStat, '> ', oMsg
        CALL zzrc( 3 )
    END IF


    REWIND oUnit


    READ(UNIT=oUnit, FMT=20, IOSTAT=oStat, IOMSG=oMsg) ptCheck
20  FORMAT(I4,2I4)

    IF (oStat <> 0) THEN
        WRITE(0, *) 'asynchValueAttr01()  READ() <', oStat, '> ', oMsg
        CALL zzrc( 4 )
    END IF


    DO i = 1, 1000
        IF ((ptList( i )%x .NE. ptCheck( i )%x)  .OR.&
            &(ptList( i )%y .NE. ptCheck( i )%y)  .OR.&
            &(ptList( i )%z .NE. ptCheck( i )%z)) THEN
            WRITE(0, *) "asynchValueAttr01()  ptList() != ptCheck(), i =", i
            WRITE(0, *) "    ptList(", i, ") = (", ptList( i ), ")"
            WRITE(0, *) "   ptCheck(", i, ") = (", ptCheck( i ), ")"
            CALL zzrc( 5 )
        END IF
    END DO


    CLOSE(UNIT=oUnit, IOSTAT=oStat, IOMSG=oMsg)
    IF (oStat <> 0) THEN
        WRITE(0, *) 'asynchValueAttr01()  CLOSE() <', oStat, '> ', oMsg
        CALL zzrc( 6 )
    END IF

END PROGRAM asynchValueAttr01


INTEGER FUNCTION SavePoint(ioUnit, block, sPt)
    USE mPoint

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: ioUnit
    LOGICAL, INTENT(IN) :: block
    TYPE(tPoint), VALUE, ASYNCHRONOUS :: sPt

    INTEGER :: wStat

    CHARACTER(LEN = 256) :: wMsg


    IF ( block ) THEN
        WRITE(UNIT=ioUnit, ASYNCHRONOUS='no',&
                &FMT=20, IOSTAT=wStat, IOMSG=wMsg) sPt
        IF (wStat <> 0) THEN
            WRITE(0, *) 'SavePoint()  WRITE() <', wStat, '> ', wMsg
        END IF

    ELSE
        WRITE(UNIT=ioUnit, ASYNCHRONOUS='yes',&
                &FMT=20, IOSTAT=wStat, IOMSG=wMsg) sPt
        IF (wStat <> 0) THEN
            WRITE(0, *) 'SavePoint()  WRITE(Asynchronous) <', wStat, '> ', wMsg
        END IF
    END IF

20  FORMAT(3(I4))


    IF (wStat <> 0) THEN
        CALL zzrc( 2 )
    END IF


    SavePoint = wStat

END FUNCTION SavePoint
