!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : aceCompAllocInit01
!*                               with DTP
!*
!*  DATE                       : November 17, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor contains a Derived Type
!*                               with an ALLOCATABLE Polymorphic Component
!*  SECONDARY FUNCTIONS TESTED : and is an Initialization Expression
!*
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
!*            o A Derived Type with an ALLOCATABLE Polymorphic Component
!*
!*  2)  Testing the usage of an Array Constructor in various contexts:
!*      * As an Initialization Expression,
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE typeMod

    IMPLICIT NONE

    TYPE, ABSTRACT :: baseType(k1)
        INTEGER, KIND :: k1

        CONTAINS

            GENERIC :: OPERATOR(/=) => TypeNotEqual
            PROCEDURE(TypeNotEqual), DEFERRED :: TypeNotEqual

    END TYPE baseType

    ABSTRACT INTERFACE
        ELEMENTAL LOGICAL FUNCTION TypeNotEqual(this, o)
            IMPORT baseType
            CLASS(baseType(8)), INTENT(in) :: this
            CLASS(baseType(8)), INTENT(in) :: o
        END FUNCTION TypeNotEqual
    END INTERFACE

    TYPE, EXTENDS(baseType) :: iType
        INTEGER(k1) :: i

        CONTAINS

            PROCEDURE, PASS :: TypeNotEqual => ITypeNotEqual

    END TYPE iType

    TYPE, EXTENDS(baseType) :: rType
        REAL(k1) :: r

        CONTAINS

            PROCEDURE, PASS :: TypeNotEqual => RTypeNotEqual

    END TYPE rType

    TYPE, EXTENDS(baseType) :: cType
        COMPLEX(k1) :: c

        CONTAINS

            PROCEDURE, PASS :: TypeNotEqual => CTypeNotEqual

    END TYPE cType

    CONTAINS

        ELEMENTAL LOGICAL FUNCTION ITypeNotEqual(this, o)
            CLASS(iType(8)), INTENT(in) :: this
            CLASS(baseType(8)), INTENT(in) :: o


            ITypeNotEqual = .TRUE.
            SELECT TYPE (o)
                TYPE IS (iType(8))
                    IF (this%i == o%i) ITypeNotEqual = .FALSE.
            END SELECT

        END FUNCTION ITypeNotEqual

        ELEMENTAL LOGICAL FUNCTION RTypeNotEqual(this, o)
            CLASS(rType(8)), INTENT(in) :: this
            CLASS(baseType(8)), INTENT(in) :: o


            RTypeNotEqual = .TRUE.
            SELECT TYPE (o)
                TYPE IS (rType(8))
                    IF (this%r == o%r) RTypeNotEqual = .FALSE.
            END SELECT

        END FUNCTION RTypeNotEqual

        ELEMENTAL LOGICAL FUNCTION CTypeNotEqual(this, o)
            CLASS(cType(8)), INTENT(in) :: this
            CLASS(baseType(8)), INTENT(in) :: o


            CTypeNotEqual = .TRUE.
            SELECT TYPE (o)
                TYPE IS (cType(8))
                    IF (this%c == o%c) CTypeNotEqual = .FALSE.
            END SELECT

        END FUNCTION CTypeNotEqual

END MODULE typeMod


MODULE baseMod
    USE typeMod

    IMPLICIT NONE

    TYPE base(l1,k1)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1

        CHARACTER(l1) :: lable
        CLASS(baseType(k1)), ALLOCATABLE :: bT( : )

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual
            PROCEDURE, PASS :: NotEqual => BaseNotEqual

    END TYPE base

    CONTAINS

        LOGICAL FUNCTION BaseNotEqual(this, o)
            CLASS(base(*,8)), INTENT(in) :: this
            CLASS(base(*,8)), INTENT(in) :: o


            BaseNotEqual = .TRUE.
            IF ((this%l1 == o%l1)       .AND.&
                (this%lable == o%lable) .AND.&
                (.NOT. ANY(this%bT /= o%bT))) THEN
                BaseNotEqual = .FALSE.
            END IF

        END FUNCTION BaseNotEqual

END MODULE baseMod


PROGRAM aceCompAllocInit01
    USE baseMod

    IMPLICIT NONE

    INTEGER :: i

    CHARACTER(3), PARAMETER :: items( 3 ) = [ 'I-1', 'I-2', 'I-3' ]

    TYPE(base(3,8)), PARAMETER :: a = base(3,8)(items( 1 ),NULL( ))
    TYPE(base(3,8)), PARAMETER :: b = base(3,8)(items( 2 ),NULL( ))
    TYPE(base(3,8)), PARAMETER :: c = base(3,8)(items( 3 ),NULL( ))

    TYPE(base(3,8)) :: d( 3 ) = [ a, base(3,8)(items( 2 ),NULL( )), c ]

    TYPE(base(3,8)) :: e
    TYPE(base(3,8)) :: f
    TYPE(base(3,8)) :: g


    IF (SIZE( d ) /= 3)                 CALL zzrc( 10_4 )

    DO i = 1, SIZE( d )
        IF ( ALLOCATED( d( i )%bT ) )   CALL zzrc( (10_4 + INT(i, 4)) )
        IF (d( i )%lable /= items( i )) CALL zzrc( (20_4 + INT(i, 4)) )
    END DO


    e = base(3,8)(items( 1 ),NULL( ))
    ALLOCATE(e%bT( 1 ), SOURCE=[ iType(8)(1_8) ])

    f = base(3,8)(items( 2 ),NULL( ))
    ALLOCATE(f%bT( 1 ), SOURCE=[ rType(8)(2.0_8) ])

    g = base(3,8)(items( 3 ),NULL( ))
    ALLOCATE(g%bT( 1 ), SOURCE=[ cType(8)((3.0_8,3.0_8)) ])

    d = [ g, f, e ]


    IF (SIZE( d ) /= 3)                     CALL zzrc( 50_4 )

    DO i = 1, 3
        IF (.NOT. ALLOCATED( d( i )%bT ))   CALL zzrc( (50_4 + INT(i, 4)) )

        IF (i == 1) THEN
            IF (d( i ) /= g)                CALL zzrc( (60_4 + INT(i, 4)) )

        ELSE IF (i == 2) THEN
            IF (d( i ) /= f)                CALL zzrc( (60_4 + INT(i, 4)) )

        ELSE
            IF (d( i ) /= e)                CALL zzrc( (60_4 + INT(i, 4)) )
        END IF
    END DO

END PROGRAM aceCompAllocInit01
