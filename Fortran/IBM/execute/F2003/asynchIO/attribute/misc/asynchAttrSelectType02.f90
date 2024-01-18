!*  ===================================================================
!*
!*                               Attribute in the SELECT TYPE Construct
!*
!*  DATE                       : March  2, 2006
!*  ORIGIN                     : AIX Compiler Development,
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
    TYPE tBase
        CHARACTER(LEN = 8) :: base
        LOGICAL :: saved
    END TYPE tBase
END MODULE mBase

MODULE mDerived1
    USE mBase

    TYPE, EXTENDS(tBase) :: tDerived1
        INTEGER :: derived
    END TYPE tDerived1
END MODULE mDerived1

MODULE mDerived2
    USE mBase

    TYPE, EXTENDS(tBase) :: tDerived2
        REAL :: derived
    END TYPE tDerived2
END MODULE mDerived2

PROGRAM asynchAttrSelectType02
    USE mDerived1
    USE mDerived2

    CHARACTER(LEN = 256) :: oMsg

    TYPE(tBase), TARGET :: base = tbase('Base',.FALSE.)
    TYPE(tDerived1), TARGET :: derived1 = tDerived1('Derived1',.FALSE.,8765)
    TYPE(tDerived2), TARGET :: derived2 = tDerived2('Derived2',.FALSE.,5.678)

    CLASS(tBase), POINTER :: basePtr


    ioUnit = 365
    OPEN(UNIT=ioUnit, ASYNCHRONOUS='yes', ACCESS='sequential',&
            &FILE='asynchAttrSelectType02.dat', ACTION='write',&
                        &FORM='formatted', IOSTAT=iStat, IOMSG=oMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", oMsg
        CALL zzrc( 1 )
    END IF


    basePtr => derived2
    iStat = Dump(ioUnit, basePtr)
    IF (iStat /= 0) THEN
        WRITE(0, *) "Dump(derived2) WRITE() <", iStat, ">"
        CALL zzrc( 2 )

    ELSE IF (.NOT. derived2.saved) THEN
        WRITE(0, *) "Dump(derived2) (.NOT. derived2.saved)"
        CALL zzrc( 3 )
    END IF


    basePtr => derived1
    iStat = Dump(ioUnit, basePtr)
    IF (iStat /= 0) THEN
        WRITE(0, *) "Dump(derived1) WRITE() <", iStat, ">"
        CALL zzrc( 4 )

    ELSE IF (.NOT. derived1.saved) THEN
        WRITE(0, *) "Dump(derived1) (.NOT. derived1.saved)"
        CALL zzrc( 5 )
    END IF


    basePtr => base
    iStat = Dump(ioUnit, basePtr)
    IF (iStat /= 0) THEN
        WRITE(0, *) "Dump(base) WRITE() <", iStat, ">"
        CALL zzrc( 6 )

    ELSE IF (.NOT. base.saved) THEN
        WRITE(0, *) "Dump(base) (.NOT. base.saved)"
        CALL zzrc( 7 )
    END IF


    CLOSE(UNIT=ioUnit, IOSTAT=iStat, IOMSG=oMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", oMsg
        CALL zzrc( 8 )
    END IF


    CONTAINS

        INTEGER FUNCTION Dump(ioUnit, theBase)
            USE mDerived1
            USE mDerived2

            INTEGER, INTENT(IN) :: ioUnit
            CLASS(tBase), POINTER, INTENT(INOUT) :: theBase

            INTEGER :: iStatus
            CHARACTER(LEN = 256) :: oMsg = ''


            SELECT TYPE (aBase => theBase)
                TYPE IS (tDerived2)
                    WRITE(ioUnit, ASYNCHRONOUS='yes', IOSTAT=iStatus,&
                        &FMT=100, IOMSG=oMsg) aBase%base, aBase%derived

                TYPE IS (tDerived1)
                    WRITE(ioUnit, ASYNCHRONOUS='yes', IOSTAT=iStatus,&
                        &FMT=200, IOMSG=oMsg) aBase%base, aBase%derived

                CLASS DEFAULT
                    WRITE(ioUnit, ASYNCHRONOUS='yes', IOSTAT=iStatus,&
                                        &FMT=300, IOMSG=oMsg) aBase%base
            END SELECT


100         FORMAT('tDerived2 = "',A8,'", Value = "',F5.3,'"')
200         FORMAT('tDerived1 = "',A8,'", Value = "',I4,'"')
300         FORMAT('tBase     = "',A8,'", No Value ')


            IF (iStatus == 0) THEN
                theBase%saved = .TRUE.

            ELSE
                WRITE(0, *) oMsg
            END IF


            Dump = iStatus

        END FUNCTION Dump

END PROGRAM asynchAttrSelectType02
