!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : May 25, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Embedded Structure Constructors
!*  SECONDARY FUNCTIONS TESTED : With automatic, ALLOCATABLE, and POINTER
!*                               variables.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE mType
    IMPLICIT NONE

    TYPE tType(kind,lowLen,highLen)
        INTEGER, KIND :: kind
        INTEGER, LEN :: lowLen
        INTEGER, LEN :: highLen

        REAL(kind) :: array( lowLen:highLen )

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual8
            PROCEDURE, PASS :: NotEqual8 => TType8NotEqual

    END TYPE tType

    CONTAINS

        LOGICAL FUNCTION TType8NotEqual(this, that)
            CLASS(tType(8,*,*)), INTENT(in) :: this
            CLASS(tType(8,*,*)), INTENT(in) :: that


            TType8NotEqual = .TRUE.
            IF ((this%kind == that%kind)                            .AND.&
                (this%lowLen == that%lowLen)                        .AND.&
                (this%highLen == that%highLen)                      .AND.&
                (LBOUND(this%array, 1) == LBOUND(that%array, 1))    .AND.&
                (UBOUND(this%array, 1) == UBOUND(that%array, 1))    .AND.&
                ( ALL(this%array == that%array) )) THEN
                TType8NotEqual = .FALSE.
            END IF

        END FUNCTION TType8NotEqual

END MODULE mType


MODULE mAnotherType
    IMPLICIT NONE

    TYPE tAnotherType(kind,highLen)
        INTEGER, KIND :: kind
        INTEGER, LEN :: highLen

        REAL(kind) :: array( highLen )

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual4
            PROCEDURE, PASS :: NotEqual4 => TAnotherType4NotEqual

    END TYPE tAnotherType

    CONTAINS

        LOGICAL FUNCTION TAnotherType4NotEqual(this, that)
            CLASS(tAnotherType(4,*)), INTENT(in) :: this
            CLASS(tAnotherType(4,*)), INTENT(in) :: that


            TAnotherType4NotEqual = .TRUE.
            IF ((this%kind == that%kind)                    .AND.&
                (this%highLen == that%highLen)              .AND.&
                (SIZE( this%array ) == SIZE( that%array ))  .AND.&
                ( ALL(this%array == that%array) )) THEN
                TAnotherType4NotEqual = .FALSE.
            END IF

        END FUNCTION TAnotherType4NotEqual

END MODULE mAnotherType


MODULE mExtendedType
    USE mType

    IMPLICIT NONE

    TYPE, EXTENDS(tType) :: tExtendedType(extKind)
        INTEGER, KIND :: extKind

        INTEGER(extKind) :: data
    END TYPE tExtendedType

    CONTAINS

        SUBROUTINE CheckTExtendedType8_2(extendedType, type, data, rc)
            TYPE(tExtendedType(8,*,*,2)) :: extendedType
            TYPE(tType(8,*,*)) :: type
            INTEGER(2) :: data
            INTEGER(4) :: rc


            IF (extendedType%data /= data)          CALL zzrc( rc )
            IF (extendedType%tType /= type)         CALL zzrc( (rc + 1_4) )

        END SUBROUTINE CheckTExtendedType8_2

END MODULE mExtendedType


MODULE mContainerType
    USE mAnotherType
    USE mExtendedType

    IMPLICIT NONE

    TYPE tContainerType(kind1,len1,len1a,kind2,len2,kind3)
        INTEGER, KIND :: kind1
        INTEGER, LEN :: len1
        INTEGER, LEN :: len1a
        INTEGER, KIND :: kind2
        INTEGER, LEN :: len2
        INTEGER, KIND :: kind3

        TYPE(tType(kind1,len1,len1a)) :: type
        TYPE(tAnotherType(kind2,len2)) :: anotherType
        TYPE(tExtendedType(kind1,len1,len1a,kind3)) :: extendedType
    END TYPE tContainerType

    CONTAINS

        SUBROUTINE CheckTContainerType8_4_2(containerType, type,&
                                            anotherType, extendedType, rc)
            TYPE(tContainerType(8,*,*,4,*,2)), POINTER :: containerType
            TYPE(tType(8,*,*)) :: type
            TYPE(tAnotherType(4,*)) :: anotherType
            TYPE(tExtendedType(8,*,*,2)) :: extendedType
            INTEGER(4) :: rc


            IF (containerType%type /= type)     CALL zzrc( rc )
            IF (containerType%anotherType /= anotherType)&
                                                CALL zzrc( (rc + 1_4) )

            CALL CheckTExtendedType8_2(containerType%extendedType,&
                                          type, extendedType%data, (rc + 2_4))

        END SUBROUTINE CheckTContainerType8_4_2

END MODULE mContainerType


MODULE mOverExtendedType
    USE mAnotherType
    USE mExtendedType

    IMPLICIT NONE

    TYPE, EXTENDS(tExtendedType) ::&
            tOverExtendedType(overExtendedKind,overExtendedLen)
        INTEGER, KIND :: overExtendedKind
        INTEGER, LEN :: overExtendedLen

        TYPE(tAnotherType(overExtendedKind,overExtendedLen)) :: anotherType
    END TYPE tOverExtendedType

END MODULE mOverExtendedType


PROGRAM dtpStructConstr02
    USE mContainerType
    USE mOverExtendedType

    IMPLICIT NONE

    INTEGER :: i
    REAL(8) :: real8Array( 2 ) = [ ((1.0_8 / REAL(i, 8)), i = 1, 2) ]


    TYPE(tType(8,3,4)) :: type
    TYPE(tType(8,3,4)) :: verifyType

    TYPE(tAnotherType(4,2)) :: anotherType
    TYPE(tAnotherType(4,2)) :: verifyAnotherType

    TYPE(tExtendedType(8,3,4,2)), ALLOCATABLE :: extendedType
    TYPE(tContainerType(8,3,4,4,2,2)), POINTER :: containerType

    TYPE(tOverExtendedType(8,:,:,2,4,:)),&
                              ALLOCATABLE, TARGET :: overExtendedType


    type = tType(8,3,4)(real8Array)

    verifyType%array = real8Array
    IF (type /= verifyType)                 ERROR STOP 10_4


    anotherType = tAnotherType(4,2)(REAL(real8Array, 4))
    verifyAnotherType%array = REAL(real8Array, 4)

    IF (anotherType /= verifyAnotherType)   ERROR STOP 20_4


    extendedType =&
        tExtendedType(8,3,4,2)(tType=tType(8,3,4)(real8Array),data=3_2)
    CALL CheckTExtendedType(extendedType, type, 3_2, 30_4)


    ALLOCATE(containerType,                                         &
             SOURCE=tContainerType(8,3,4,4,2,2)                     &
                        (tType(8,3,4)(real8Array),                  &
                         tAnotherType(4,2)(REAL(real8Array, 4)),    &
                         tExtendedType(8,3,4,2)(tType=tType(8,3,4)  &
                                                (real8Array),data=3_2)))
    CALL CheckTContainerType(containerType, type,&
                                    anotherType, extendedType, 40_4)


    overExtendedType =                                   &
        tOverExtendedType(8,3,4,2,4,2)(                     &
            tExtendedType=tExtendedType(8,3,4,2)(           &
                tType=tType(8,3,4)(real8Array),data=3_2),   &
                    anotherType=tAnotherType(4,2)(REAL(real8Array, 4)))

    CALL CheckTOverExtendedType(overExtendedType,&
                                extendedType, anotherType, 50_4)


    CONTAINS

        SUBROUTINE CheckTExtendedType(extendedType, type, data, rc)
            TYPE(tExtendedType(8,*,*,2)), ALLOCATABLE :: extendedType
            TYPE(tType(8,*,*)) :: type
            INTEGER(2) :: data
            INTEGER(4) :: rc


            IF (.NOT. ALLOCATED( extendedType ) )   CALL zzrc( rc )
            CALL CheckTExtendedType8_2(extendedType, type, data, (rc + 1_4))

        END SUBROUTINE CheckTExtendedType


        SUBROUTINE CheckTContainerType(containerType, type,&
                                        anotherType, extendedType, rc)
            TYPE(tContainerType(8,*,*,4,*,2)), POINTER :: containerType
            TYPE(tType(8,*,*)) :: type
            TYPE(tAnotherType(4,*)) :: anotherType
            TYPE(tExtendedType(8,*,*,2)) :: extendedType
            INTEGER(4) :: rc


            IF (.NOT. ASSOCIATED( containerType ))  CALL zzrc( rc )
            CALL CheckTContainerType8_4_2(containerType, type,&
                                            anotherType, extendedType,&
                                                                (rc + 1_4))

        END SUBROUTINE CheckTContainerType


        SUBROUTINE CheckTOverExtendedType(overExtendedType,&
                                            extendedType, anotherType, rc)
            TYPE(tOverExtendedType(8,:,:,2,4,:)),&
                                    ALLOCATABLE :: overExtendedType
            TYPE(tExtendedType(8,*,*,2)) :: extendedType
            TYPE(tAnotherType(4,*)) :: anotherType
            INTEGER(4) :: rc


            IF (.NOT. ALLOCATED( overExtendedType ) )   CALL zzrc( rc )
            IF (overExtendedType%anotherType /= anotherType)&
                                                        CALL zzrc( (rc + 1_4) )

            CALL CheckTExtendedType8_2(&
                        overExtendedType%tExtendedType,&
                        extendedType%tType, extendedType%data, (rc + 2_4))

        END SUBROUTINE CheckTOverExtendedType

END PROGRAM dtpStructConstr02
