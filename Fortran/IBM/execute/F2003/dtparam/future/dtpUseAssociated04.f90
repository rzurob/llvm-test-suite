!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : July  7, 2009 (edited on August 21, 2009)
!*
!*  PRIMARY FUNCTIONS TESTED   : A MOUDLE is USEd by two other MODULEs
!*  SECONDARY FUNCTIONS TESTED : The other MODULEs are USEd by a Subprogram -- track defect 367200
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  CHANGES:
!*  The case tests the use of derived type parameters in TPINQ expressions, which
!*  are sometimes not recognized as constants or may be incorrectly initialized if
!*  we are looking at the attributes of a declared variable, whether or not it has
!*  been initialized.  A workaround has been developed which involves creating a
!*  named constant (parameter) initialized with a value compatible with the original
!*  variable, and querying its attributes instead.  Sometimes this is not enough, and
!*  we then need to created an integer parameter which is initialized with the
!*  desired derived type parameter of the constant (parameter) we already created.
!*  See defect 367200 for details.
!*  This is the original test case, unchanged.

!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE mBase
    IMPLICIT NONE

    TYPE tBase(arrayDim1,arrayDim2,arrayKind)
        INTEGER, LEN :: arrayDim1
        INTEGER, LEN :: arrayDim2
        INTEGER, KIND :: arrayKind

        REAL(arrayKind) :: array( arrayDim1,arrayDim2 )

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual
            PROCEDURE, PASS :: NotEqual => TBaseNotEqual

    END TYPE tBase


    TYPE(tBase(3,4,16)), TARGET :: base( 10 )


    CONTAINS

        LOGICAL FUNCTION TBaseNotEqual(this, that)
            CLASS(tBase(*,*,16)), INTENT(in) :: this
            CLASS(tBase(*,*,16)), INTENT(in) :: that


            TBaseNotEqual = .TRUE.
            IF ((this%arrayDim1 == that%arrayDim1)      .AND.&
                (this%arrayDim2 == that%arrayDim2)      .AND.&
                (this%arrayKind == that%arrayKind)      .AND.&
                (SIZE(this%array, 1) == this%arrayDim1) .AND.&
                (SIZE(this%array, 2) == this%arrayDim2) .AND.&
                (SIZE(that%array, 1) == this%arrayDim1) .AND.&
                (SIZE(that%array, 2) == this%arrayDim2) .AND.&
                ( ALL(this%array == that%array) )) THEN
                TBaseNotEqual = .FALSE.
            END IF

        END FUNCTION TBaseNotEqual

END MODULE mBase


MODULE mVariation1
    USE mBase

    IMPLICIT NONE

    TYPE, EXTENDS(tBase) :: tVariation1(variation1Kind)
        INTEGER, KIND :: variation1Kind

        REAL(variation1Kind) :: variation1Array( arrayDim1,arrayDim2 )
    END TYPE tVariation1


    TYPE(tVariation1(:,:,16,8)), POINTER :: variation1( : )

END MODULE mVariation1


MODULE mVariation2
    USE mBase

    IMPLICIT NONE

    TYPE, EXTENDS(tBase) :: tVariation2(variation2Kind)
        INTEGER, KIND :: variation2Kind

        INTEGER(variation2Kind) :: variation2Array( arrayDim1,arrayDim2 )
    END TYPE tVariation2

END MODULE mVariation2


PROGRAM dtpUseAssociated04
    USE mVariation1
    USE mVariation2

    IMPLICIT NONE


    INTERFACE
        SUBROUTINE DoubleUse( rc )
            INTEGER(4) :: rc
        END SUBROUTINE DoubleUse
    END INTERFACE


    INTEGER :: i
    INTEGER :: j

    INTEGER :: stat
    CHARACTER(255) :: errmsg

    REAL(base%arrayKind), ALLOCATABLE :: realArray( : )


    ALLOCATE(realArray( (base%arrayDim1 * base%arrayDim2 * SIZE( base )) ), &
            STAT=stat, ERRMSG=errmsg,                                       &
            SOURCE=[ ((1.0_16 / REAL(i, base%arrayKind)),                   &
                    i = 1, (base%arrayDim1 * base%arrayDim2 * SIZE( base ))) ])

    IF (stat /= 0) THEN
        WRITE(1, *) "ALLOCATE(STAT=", stat, "): ", LEN_TRIM( errmsg )
        CALL zzrc( 5_4 )
    END IF


    IF (.NOT. ALLOCATED( realArray ))                   CALL zzrc( 10_4 )
    IF (KIND( realArray ) /= 16)                        CALL zzrc( 11_4 )
    IF (SIZE( realArray ) /= 120)                       CALL zzrc( 12_4 )

    IF ( ANY(realArray /= [ ((1.0_16 / REAL(i, 16)), i = 1, 120) ]) )&
                                                        CALL zzrc( 13_4 )

    DO i = 1, 10
        j = ((i - 1) * 12) + 1
        base( i ) =                                             &
            tBase(base%arrayDim1,base%arrayDim2,base%arrayKind) &
                        (RESHAPE(realArray( j:(j + 11) ),       &
                                [ base%arrayDim1,base%arrayDim2 ]))
    END DO


    CALL VerifyBase(RESHAPE(realArray, [ 3,4,10 ]), 20_4)

    CALL DoubleUse( 30_4 )


    CONTAINS

        SUBROUTINE VerifyBase(realArray, rc)
            REAL(16) :: realArray( :,:,: )
            INTEGER(4) :: rc

            INTEGER :: i


            DO i = 1, 10
                IF ( ANY(base( i )%array /= realArray( :,:,i )) )   &
                                            CALL zzrc( (rc + INT(i, 4)) )
            END DO

        END SUBROUTINE VerifyBase

END PROGRAM dtpUseAssociated04


SUBROUTINE DoubleUse( rc )
    USE mVariation2
    USE mVariation1

    IMPLICIT NONE

    INTEGER(4) :: rc


    INTEGER :: i

    INTEGER :: stat
    CHARACTER(255) :: errmsg

    TYPE(tBase(base%arrayDim1,base%arrayDim2,base%arrayKind)),&
                                                    POINTER :: pBase( : )
    TYPE(tVariation2(base%arrayDim1,base%arrayDim2,base%arrayKind,4))&
                                            :: variation2( base%arrayDim1 )


    pBase => base( :base%arrayDim2 )
    ALLOCATE(tVariation1(base%arrayDim1,base%arrayDim2,         &
                variation1%arrayKind,variation1%variation1Kind) &
                    :: variation1( base%arrayDim2 ), STAT=stat, ERRMSG=errmsg)
    IF (stat /= 0) THEN
        WRITE(1, *) "ALLOCATE(STAT=", stat, "): ", LEN_TRIM( errmsg )
        CALL zzrc( rc )
    END IF


    IF (.NOT. ASSOCIATED( variation1 ))             CALL zzrc( (rc + 1_4) )
    IF (SIZE( variation1 ) /= 4)                    CALL zzrc( (rc + 2_4) )

    DO i = 1, base%arrayDim2
        variation1( i )%array = pBase( i )%array
        variation1( i )%variation1Array =&
            REAL(variation1( i )%array, variation1%variation1Kind)
    END DO



    CALL VerifyVariation1(variation1, 100_4)


    pBase => base( :base%arrayDim1 )
    DO i = 1, base%arrayDim1
        variation2( i ) =                                       &
            tVariation2(base%arrayDim1,base%arrayDim2,          &
                variation2%arrayKind,variation2%variation2Kind) &
                    (tBase=pBase( i ),                          &
                     variation2Array=INT((pBase( i )%array * 1000),4))
    END DO

    IF (SIZE( variation2 ) /= 3)                    CALL zzrc( (rc + 3_4) )

    CALL VerifyVariation2( 100_4 )


    CONTAINS

        SUBROUTINE VerifyVariation1(var1, rc)
            TYPE(tVariation1(base%arrayDim1,base%arrayDim2,&
                    base%arrayKind,variation1%variation1Kind)) :: var1( : )
            INTEGER(4) :: rc

            INTEGER :: i


            DO i = 1, 4
                IF (SIZE(var1( i )%array, 1) /= 3)              &
                                        CALL zzrc( (rc + INT(i, 4)) )
                IF (SIZE(var1( i )%array, 2) /= 4)              &
                                        CALL zzrc( (rc + 10_4 + INT(i, 4)) )
                IF (SIZE(var1( i )%variation1Array, 1) /= 3)    &
                                        CALL zzrc( (rc + 20_4 + INT(i, 4)) )
                IF (SIZE(var1( i )%variation1Array, 2) /= 4)    &
                                        CALL zzrc( (rc + 30_4 + INT(i, 4)) )
                IF (KIND( var1( i )%variation1Array ) /= 8)     &
                                        CALL zzrc( (rc + 40_4 + INT(i, 4)) )

                IF (var1( i )%tBase /= base( i ))               &
                                        CALL zzrc( (rc + 50_4 + INT(i, 4)) )

                IF ( ANY(var1( i )%variation1Array /=           &
                            REAL(base( i )%array, 8)) )         &
                                        CALL zzrc( (rc + 60_4 + INT(i, 4)) )
            END DO

        END SUBROUTINE VerifyVariation1

        SUBROUTINE VerifyVariation2( rc )
            INTEGER(4) :: rc

            INTEGER :: i


            DO i = 1, 3
                IF (SIZE(variation2( i )%array, 1) /= 3)            &
                                        CALL zzrc( (rc + 5_4 + INT(i, 4)) )
                IF (SIZE(variation2( i )%array, 2) /= 4)            &
                                        CALL zzrc( (rc + 15_4 + INT(i, 4)) )
                IF (SIZE(variation2( i )%variation2Array, 1) /= 3)  &
                                        CALL zzrc( (rc + 25_4 + INT(i, 4)) )
                IF (SIZE(variation2( i )%variation2Array, 2) /= 4)  &
                                        CALL zzrc( (rc + 35_4 + INT(i, 4)) )
                IF (KIND( variation2( i )%variation2Array ) /= 4)   &
                                        CALL zzrc( (rc + 45_4 + INT(i, 4)) )

                IF (variation2( i )%tBase /= base( i ))             &
                                        CALL zzrc( (rc + 55_4 + INT(i, 4)) )

                IF ( ANY(variation2( i )%variation2Array /=         &
                            INT((base( i )%array * 1000), 4)) )     &
                                        CALL zzrc( (rc + 65_4 + INT(i, 4)) )
            END DO

        END SUBROUTINE VerifyVariation2

END SUBROUTINE DoubleUse
