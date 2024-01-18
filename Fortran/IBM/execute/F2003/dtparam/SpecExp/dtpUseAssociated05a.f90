!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : dtpUseAssociated05a
!*
!*  DATE                       : July 13, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Two MODULEs USE Associated with a Subprogram
!*                               with a common USE Associated MOUDLE wtih
!*                               Renaming and Default Initialization
!*  SECONDARY FUNCTIONS TESTED : Where specification-exprs contain simple
!*                               calculations
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE mBase
    IMPLICIT NONE

    TYPE tBase(arrayDim1,arrayDim2,arrayKind)
        INTEGER, LEN :: arrayDim1 = 3
        INTEGER, LEN :: arrayDim2 = 4
        INTEGER, KIND :: arrayKind

        REAL(arrayKind) :: array( arrayDim1,arrayDim2 )

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual
            PROCEDURE, PASS :: NotEqual => TBaseNotEqual

    END TYPE tBase


    TYPE(tBase(arrayKind=16)), SAVE, TARGET :: base( 10 )


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
    USE mBase, variation1Base => base

    IMPLICIT NONE

    TYPE, EXTENDS(tBase) :: tVariation1(variation1Kind)
        INTEGER, KIND :: variation1Kind

        REAL(variation1Kind) :: variation1Array( arrayDim1,arrayDim2 )
    END TYPE tVariation1


    TYPE(tVariation1(:,:,16,8)), POINTER :: variation1( : )

END MODULE mVariation1


MODULE mVariation2
    USE mBase, variation2Base => base

    IMPLICIT NONE

    TYPE, EXTENDS(tBase) :: tVariation2(variation2Kind)
        INTEGER, KIND :: variation2Kind

        INTEGER(variation2Kind) :: variation2Array( arrayDim1,arrayDim2 )
    END TYPE tVariation2

END MODULE mVariation2


PROGRAM dtpUseAssociated05a
    USE mVariation1, base1 => variation1Base
    USE mVariation2, base2 => variation2Base

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

    REAL(base1%arrayKind), ALLOCATABLE :: realArray( : )


    ALLOCATE(realArray( (base2%arrayDim1 * base2%arrayDim2 * SIZE( base2 )) ),&
            STAT=stat, ERRMSG=errmsg, SOURCE=[ ((1.0_16 / REAL(i, 16)), i = 1,&
                (base1%arrayDim1 * base1%arrayDim2 * SIZE( base1 ))) ])

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
        base2( i ) =                                                          &
            tBase((base2%arrayDim2 - 1),(base2%arrayDim1 + 1),base2%arrayKind)&
            (RESHAPE(realArray( j:(j + 11) ),                                 &
                                        [ base1%arrayDim1,base1%arrayDim2 ]))
    END DO


    CALL VerifyBase(RESHAPE(realArray, [ 3,4,10 ]), 20_4)

    CALL DoubleUse( 30_4 )


    CONTAINS

        SUBROUTINE VerifyBase(realArray, rc)
            REAL(16) :: realArray( :,:,: )
            INTEGER(4) :: rc

            INTEGER :: i


            DO i = 1, 10
                IF ( ANY(base2( i )%array /= realArray( :,:,i )) )   &
                                            CALL zzrc( (rc + INT(i, 4)) )
            END DO

        END SUBROUTINE VerifyBase

END PROGRAM dtpUseAssociated05a


SUBROUTINE DoubleUse( rc )
    USE mVariation2, base2 => variation2Base
    USE mVariation1, base1 => variation1Base

    IMPLICIT NONE

    INTEGER(4) :: rc


    INTEGER :: i

    INTEGER :: stat
    CHARACTER(255) :: errmsg

    TYPE(tBase(base2%arrayDim1,base2%arrayDim2,base2%arrayKind)),&
                                                      POINTER :: pBase( : )
    TYPE(tVariation2(                                                   &
        (base1%arrayDim2 - 1),(base1%arrayDim1 + 1),base1%arrayKind,4)) ::&
                    variation2( (base2%arrayDim1 - 2):(base2%arrayDim2 - 1) )


    pBase => base1( :base1%arrayDim2 )
    ALLOCATE(tVariation1(base2%arrayDim1,base2%arrayDim2,         &
                variation1%arrayKind,variation1%variation1Kind) &
                    :: variation1( base1%arrayDim2 ), STAT=stat, ERRMSG=errmsg)
    IF (stat /= 0) THEN
        WRITE(1, *) "ALLOCATE(STAT=", stat, "): ", LEN_TRIM( errmsg )
        CALL zzrc( rc )
    END IF


    IF (.NOT. ASSOCIATED( variation1 ))             CALL zzrc( (rc + 1_4) )
    IF (SIZE( variation1 ) /= 4)                    CALL zzrc( (rc + 2_4) )

    DO i = 1, base2%arrayDim2
        variation1( i )%array = pBase( i )%array
        variation1( i )%variation1Array = REAL(variation1( i )%array, 16)
    END DO



    CALL VerifyVariation1(variation1, 100_4)


    pBase => base1( :base1%arrayDim1 )
    DO i = 1, base2%arrayDim1
        variation2( i ) =                                       &
            tVariation2(base1%arrayDim1,base2%arrayDim2,        &
                variation2%arrayKind,variation2%variation2Kind) &
                    (tBase=pBase( i ),                          &
                     variation2Array=INT((pBase( i )%array * 1000),4))
    END DO

    IF (SIZE( variation2 ) /= 3)                    CALL zzrc( (rc + 3_4) )

    CALL VerifyVariation2( 100_4 )


    CONTAINS

        SUBROUTINE VerifyVariation1(var1, rc)
            TYPE(tVariation1(((base1%arrayDim2 * 3) / 4),   &
                             ((base1%arrayDim1 * 4) / 3),   &
                    base2%arrayKind,variation1%variation1Kind)) :: var1( : )
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

                IF (var1( i )%tBase /= base1( i ))              &
                                        CALL zzrc( (rc + 50_4 + INT(i, 4)) )

                IF ( ANY(var1( i )%variation1Array /=           &
                            REAL(base2( i )%array, 8)) )        &
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

                IF (variation2( i )%tBase /= base1( i ))            &
                                        CALL zzrc( (rc + 55_4 + INT(i, 4)) )

                IF ( ANY(variation2( i )%variation2Array /=         &
                            INT((base2( i )%array * 1000), 4)) )    &
                                        CALL zzrc( (rc + 65_4 + INT(i, 4)) )
            END DO

        END SUBROUTINE VerifyVariation2

END SUBROUTINE DoubleUse
