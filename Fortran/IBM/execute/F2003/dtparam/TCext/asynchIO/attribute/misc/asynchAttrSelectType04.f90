! GB DTP extension using:
! ftcx_dtp -ql -qreuse=none /tstdev/F2003/asynchIO/attribute/misc/asynchAttrSelectType04.f
! opt variations: -qnol -qreuse=base

!*  ===================================================================
!*
!*                               Attribute in the SELECT TYPE Construct
!*
!*  DATE                       : March  3, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute
!*  SECONDARY FUNCTIONS TESTED : associate-name => selector (where selector
!*                               implicitly has the ASYNCHRONOUS Attribute)
!*
!*  REQUIRED COMPILER OPTIONS  : -qattr=full
!*
!*  KEYWORD(S)                 : ASYNCHRONOUS Attribute, SELECT TYPE Construct
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  8.1.4.3 Attributes of associate names
!*
!*  Within a SELECT TYPE or ASSOCIATE construct, ... The associating entity
!*  has the ASYNCHRONOUS, TARGET, or VOLATILE attribute if and only if the
!*  selector is a variable and has the attribute.
!*
!*  8.1.5.1 Form of the SELECT TYPE construct
!*
!*  R821 select-type-construct  is  select-type-stmt
!*                                      [ type-guard-stmt
!*                                        block ] ...
!*                                      end-select-type-stmt
!*  R822 select-type-stmt       is  [ select-construct-name : ] SELECT TYPE&
!*                                      &( [ associate-name => ] selector )
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mBase
    TYPE tBase(N1,K1)    ! (20,4)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
        INTEGER(K1)   :: base
    END TYPE tBase
END MODULE mBase


MODULE mDerived1
    USE mBase

    TYPE, EXTENDS(tBase) :: tDerived1(N2,K2)    ! (20,4,20,4)
        INTEGER, KIND :: K2
        INTEGER, LEN  :: N2
        INTEGER(K2)   :: derived1
    END TYPE tDerived1
END MODULE mDerived1


MODULE mDerived2
    USE mDerived1

    TYPE, EXTENDS(tDerived1) :: tDerived2(N3,K3)    ! (20,4,20,4,20,4)
        INTEGER, KIND :: K3
        INTEGER, LEN  :: N3
        INTEGER(K3)   :: derived2
    END TYPE tDerived2
END MODULE mDerived2


PROGRAM asynchAttrSelectType04
    USE mDerived2

    INTERFACE
        INTEGER FUNCTION Init(ioUnit, iMsg, anObj)
            USE mDerived2

            INTEGER, INTENT(IN) :: ioUnit
            CHARACTER(LEN = 256), INTENT(OUT) :: iMsg
            CLASS(tBase(*,4)), INTENT(OUT) :: anObj
        END FUNCTION Init
    END INTERFACE

    CHARACTER(LEN = 256) :: iMsg

    TYPE(tBase(20,4)) :: base
    TYPE(tDerived1(20,4,20,4)) :: derived1
    TYPE(tDerived2(20,4,20,4,20,4)) :: derived2


    ioUnit = 8
    OPEN(ioUnit, FILE='asynchAttrSelectType04.dat',&
        &ACTION='read', FORM='formatted', ACCESS='stream',&
                    &ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    iStat = Init(ioUnit, iMsg, base)
    IF (iStat == 0) THEN
        iStat = Init(ioUnit, iMsg, derived1)

        IF (iStat == 0) THEN
            iStat = Init(ioUnit, iMsg, derived2)
        END IF
    END IF

    IF (iStat <> 0) THEN
        WRITE(0, *) "Init() <", iStat, "> ", iMsg
        CALL zzrc( 2 )
    END IF


    IF (iStat == 0) THEN
        WAIT(ioUnit, IOSTAT=iStat, IOMSG=iMsg)
        IF (iStat <> 0) THEN
            WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
            CALL zzrc( 3 )
        END IF
    END IF


    PRINT 100, base%base
    PRINT 200, derived1%base, derived1%derived1
    PRINT 300, derived2%base, derived2%derived1, derived2%derived2

100 FORMAT('base%base     = "',I4,'"')
200 FORMAT('derived1%base = "',I4,'", derived1%derived1 = "',I4,'"')
300 FORMAT('derived2%base = "',I4,'", derived2%derived1 = "',I4,&
                                &'", derived2%derived2 = "',I4,'"')


    CLOSE(ioUnit, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 4 )
    END IF

END PROGRAM asynchAttrSelectType04


INTEGER FUNCTION Init(ioUnit, iMsg, anObj)
    USE mDerived2

    INTEGER, INTENT(IN) :: ioUnit
    CHARACTER(LEN = 256), INTENT(OUT) :: iMsg
    CLASS(tBase(*,4)), ASYNCHRONOUS, INTENT(OUT) :: anObj

    INTEGER :: iStat


    SELECT TYPE (theObj => anObj)
        TYPE IS (tBase(*,4))
            READ(ioUnit, '(I4)', ASYNCHRONOUS='yes',&
                &IOSTAT=iStat, IOMSG=iMSG) theObj%base

        TYPE IS (tDerived1(*,4,*,4))
            READ(ioUnit, '(2I4)', ASYNCHRONOUS='no', IOSTAT=iStat,&
                            &IOMSG=iMsg) theObj%base, theObj%derived1

        TYPE IS (tDerived2(*,4,*,4,*,4))
            READ(ioUnit, '(3I4)', ASYNCHRONOUS='yes', IOSTAT=iStat,&
                &IOMSG=iMsg) theObj%base, theObj%derived1, theObj%derived2

        CLASS DEFAULT
            iStat = 1
            iMsg = 'Unknown Type'
    END SELECT


    Init = iStat

END FUNCTION Init
