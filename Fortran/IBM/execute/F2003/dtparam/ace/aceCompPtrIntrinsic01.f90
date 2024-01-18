!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : aceCompPtrIntrinsic01
!*  TEST CASE TITLE            : Basic (non-Polymorphic) Array Constructors
!*                               with DTP
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : November 25, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor contains a Derived Type
!*                               with a Polymorphic POINTER Component
!*  SECONDARY FUNCTIONS TESTED : and is the Actual Argument to an Intrinsic
!*                               Function
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  1)  Testing the format of the Array Constructor:
!*      * Brackets:  []
!*      * Where the ac-value-list contains:
!*        o A Derived Type with a Polymorphic POINTER Component
!*
!*  2)  Testing the usage of an Array Constructor in various contexts:
!*      * As the source-expr of a SOURCE= Specifier in an ALLOCATE
!*        Statement,
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE typeMod

    IMPLICIT NONE

    TYPE, ABSTRACT :: baseType(l1)
        INTEGER, LEN :: l1

        CONTAINS

            GENERIC :: OPERATOR(/=) => TypeNotEqual
            PROCEDURE(TypeNotEqual), DEFERRED :: TypeNotEqual

    END TYPE baseType

    ABSTRACT INTERFACE
        ELEMENTAL LOGICAL FUNCTION TypeNotEqual(this, o)
            IMPORT baseType
            CLASS(baseType(*)), INTENT(in) :: this
            CLASS(baseType(*)), INTENT(in) :: o
        END FUNCTION TypeNotEqual
    END INTERFACE

    TYPE, EXTENDS(baseType) :: iType
        INTEGER :: i( l1 )

        CONTAINS

            PROCEDURE, PASS :: TypeNotEqual => ITypeNotEqual

    END TYPE iType

    TYPE, EXTENDS(baseType) :: rType
        REAL :: r( l1 )

        CONTAINS

            PROCEDURE, PASS :: TypeNotEqual => RTypeNotEqual

    END TYPE rType

    TYPE, EXTENDS(baseType) :: cType
        COMPLEX :: c( l1 )

        CONTAINS

            PROCEDURE, PASS :: TypeNotEqual => CTypeNotEqual

    END TYPE cType

    CONTAINS

        ELEMENTAL LOGICAL FUNCTION ITypeNotEqual(this, o)
            CLASS(iType(*)), INTENT(in) :: this
            CLASS(baseType(*)), INTENT(in) :: o


            ITypeNotEqual = .TRUE.
            SELECT TYPE (o)
                TYPE IS (iType(*))
                    IF ((this%l1 == o%l1)   .AND.&
                        ( ALL(this%i == o%i) )) THEN
                        ITypeNotEqual = .FALSE.
                    END IF
            END SELECT

        END FUNCTION ITypeNotEqual

        ELEMENTAL LOGICAL FUNCTION RTypeNotEqual(this, o)
            CLASS(rType(*)), INTENT(in) :: this
            CLASS(baseType(*)), INTENT(in) :: o


            RTypeNotEqual = .TRUE.
            SELECT TYPE (o)
                TYPE IS (rType(*))
                    IF ((this%l1 == o%l1)   .AND.&
                        ( ALL(this%r == o%r) )) THEN
                        RTypeNotEqual = .FALSE.
                    END IF
            END SELECT

        END FUNCTION RTypeNotEqual

        ELEMENTAL LOGICAL FUNCTION CTypeNotEqual(this, o)
            CLASS(cType(*)), INTENT(in) :: this
            CLASS(baseType(*)), INTENT(in) :: o


            CTypeNotEqual = .TRUE.
            SELECT TYPE (o)
                TYPE IS (cType(*))
                    IF ((this%l1 == o%l1)   .AND.&
                        ( ALL(this%c == o%c) )) THEN
                        CTypeNotEqual = .FALSE.
                    END IF
            END SELECT

        END FUNCTION CTypeNotEqual

END MODULE typeMod


MODULE baseMod
    USE typeMod

    IMPLICIT NONE

    TYPE base(l1)
        INTEGER, LEN :: l1

        CLASS(baseType(l1)), POINTER :: bT

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual
            PROCEDURE, PASS :: NotEqual => BaseNotEqual

    END TYPE base

    CONTAINS

        ELEMENTAL LOGICAL FUNCTION BaseNotEqual(this, o)
            CLASS(base(*)), INTENT(in) :: this
            CLASS(base(*)), INTENT(in) :: o


            BaseNotEqual = .TRUE.
            IF ((this%l1 == o%l1)           .AND.&
                ( ASSOCIATED( o%bT ) )      .AND.&
                ( ASSOCIATED( this%bT ) )) THEN
                IF (.NOT. (this%bT /= o%bT)) BaseNotEqual = .FALSE.
            END IF

        END FUNCTION BaseNotEqual

END MODULE baseMod


PROGRAM aceCompPtrIntrinsic01
    USE baseMod

    IMPLICIT NONE

    INTEGER :: i

    INTEGER, PARAMETER   :: iA( 3 ) = [ (INT(i, 8), i = 1, 3) ]
    REAL, PARAMETER      :: rA( 6 ) = [ ((1.0_8 / REAL(i, 8)), i = 1, 6) ]
    COMPLEX, PARAMETER   :: cA( 3 ) =&
                        [ (CMPLX(rA( i ), rA( (i + 1) ), 8), i = 1, 6, 2) ]

    TYPE(iType(3)), TARGET :: iT = iType(3)(iA)
    TYPE(rType(3)), TARGET :: rT = rType(3)(rA( 1:3 ))
    TYPE(cType(3)), TARGET :: cT = cType(3)(cA)

    TYPE(base(3)), TARGET :: b1, b2, b3

    TYPE(base(:)), ALLOCATABLE :: bA( : )


    ALLOCATE(b1%bT, SOURCE=iT)
    ALLOCATE(b2%bT, SOURCE=rT)
    ALLOCATE(b3%bT, SOURCE=cT)


    bA = CSHIFT([ b1, b2, b3 ], 9)
    CALL VerifyBA( 10_4 )
    DEALLOCATE( bA )


    IF (COUNT([ b1, b2, b1 ] /= b3) /= 3) CALL zzrc( 20_4 )


    CONTAINS

        SUBROUTINE VerifyBA( rc )
            INTEGER(4) :: rc

            IF (.NOT. ALLOCATED( bA )) THEN
                CALL zzrc( rc )

            ELSE IF ( ANY(SHAPE( bA ) /= [ 3 ]) ) THEN
                CALL zzrc( (rc + 1_4) )

            ELSE IF ( ANY(bA /= [ b1, b2, b3 ]) ) THEN
                CALL zzrc( (rc + 2_4) )

            ELSE IF (bA( 1 ) /= b1) THEN
                CALL zzrc( (rc + 3_4) )

            ELSE IF (bA( 2 ) /= b2) THEN
                CALL zzrc( (rc + 4_4) )

            ELSE IF (bA( 3 ) /= b3) THEN
                CALL zzrc( (rc + 5_4) )
            END IF

        END SUBROUTINE VerifyBA

END PROGRAM aceCompPtrIntrinsic01
