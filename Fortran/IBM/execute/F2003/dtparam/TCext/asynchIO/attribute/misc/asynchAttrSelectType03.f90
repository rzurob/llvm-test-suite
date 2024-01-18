! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp -qreuse=base /tstdev/F2003/asynchIO/attribute/misc/asynchAttrSelectType03.f
! opt variations: -qnock -qnol -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*                               Attribute in the SELECT TYPE Construct
!*
!*  DATE                       : March  2, 2006
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

    TYPE, EXTENDS(tBase) :: tDerived1(K2,N2)    ! (20,4,1,1)
        INTEGER, KIND             :: K2
        INTEGER, LEN              :: N2
        INTEGER(K1)               :: derived1
        CHARACTER(kind=K2,len=N2) :: newline
    END TYPE tDerived1
END MODULE mDerived1


PROGRAM asynchAttrSelectType03
    USE mDerived1

    INTERFACE
        INTEGER FUNCTION Load(ioUnit, n, d1)
            USE mDerived1

            INTEGER, INTENT(IN) :: ioUnit
            INTEGER, INTENT(IN) :: n
            CLASS(tBase(:,4)), POINTER, DIMENSION( : ), INTENT(INOUT) :: d1
        END FUNCTION Load
    END INTERFACE

    CHARACTER(LEN = 256) :: iMsg

    TYPE(tDerived1(:,4,1,:)), ALLOCATABLE, TARGET, DIMENSION( : ) :: derived1
    CLASS(tBase(:,4)), POINTER, DIMENSION( : ) :: base


    ioUnit = 128
    OPEN(ioUnit, ASYNCHRONOUS='yes', FORM='formatted',&
        &FILE='asynchAttrSelectType03.dat', ACTION='read',&
            &ACCESS='sequential', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    ALLOCATE(tDerived1(20,4,1,1)::derived1( 1000 ), STAT=iStat, ERRMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "ALLOCATE() <", iStat, "> ", iMsg
        CALL zzrc( 2 )
    END IF


    DO i = 1, 1000, 100
        base => derived1( i:(i + 99) )
        iStat = Load(ioUnit, 100, base)

        IF (iStat <> 0) THEN
            CALL zzrc( 3 )
        END IF
    END DO


    CLOSE(ioUnit, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 4 )
    END IF


    DO i = 1, 1000
        PRINT "(I4,') ',I5,I5)", i,&
                &derived1( i )%base, derived1( i )%derived1
    END DO

END PROGRAM asynchAttrSelectType03


INTEGER FUNCTION Load(ioUnit, n, d1)
    USE mDerived1

    INTEGER, INTENT(IN) :: ioUnit
    INTEGER, INTENT(IN) :: n
    CLASS(tBase(:,4)), POINTER, DIMENSION( : ), INTENT(INOUT) :: d1

    CHARACTER(LEN = 256) :: iMsg


    iStat = 0
    SELECT TYPE (tD1 => d1)
        TYPE IS (tDerived1(*,4,1,*))
            PRINT *, "READ(tDerived1)"
            READ(UNIT=ioUnit, FMT='(I5,I5,A1)',&
                &ASYNCHRONOUS='yes',IOSTAT=iStat, IOMSG=iMsg)&
                &(tD1( i )%base, tD1( i )%derived1, tD1( i )%newline, i = 1, n)

        CLASS DEFAULT
            PRINT *, "READ(Default)"
            READ(UNIT=ioUnit, FMT='(I5,A1)',&
                &ASYNCHRONOUS='yes',IOSTAT=iStat, IOMSG=iMsg)&
                &(tD1( i )%base, i = 1, n)
    END SELECT


    IF (iStat <> 0) THEN
        WRITE(0, *) "READ() <", iStat, "> ", iMsg
    END IF


    Load = iStat

END FUNCTION Load
