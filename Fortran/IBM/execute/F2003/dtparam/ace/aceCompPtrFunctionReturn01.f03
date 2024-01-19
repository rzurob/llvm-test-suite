!***********************************************************************
!* =====================================================================
!*
!*                               with DTP
!*
!*  DATE                       : November 21, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor contains a Derived Type
!*                               with a Non-Polymorphic POINTER Component
!*  SECONDARY FUNCTIONS TESTED : and is a FUNCTION Return Value
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
!*        o A Derived Type with a POINTER Component (Non-Polymorphic)
!*
!*  2)  Testing the usage of an Array Constructor in various contexts:
!*      * As a FUNCTION Return value,
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE typeMod

    IMPLICIT NONE

    TYPE :: baseType(k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        INTEGER(k1) :: a( l1 )

        CONTAINS

            GENERIC :: OPERATOR(/=) => BaseTypeNotEqual
            PROCEDURE, PASS :: BaseTypeNotEqual

    END TYPE baseType

    CONTAINS

        ELEMENTAL LOGICAL FUNCTION BaseTypeNotEqual(this, o)
            CLASS(baseType(8,*)), INTENT(in) :: this
            CLASS(baseType(8,*)), INTENT(in) :: o


            BaseTypeNotEqual = .TRUE.
            IF ((this%l1 == o%l1)    .AND.&
                ( ALL(this%a == o%a) )) THEN
                BaseTypeNotEqual = .FALSE.
            END IF

        END FUNCTION BaseTypeNotEqual

END MODULE typeMod


MODULE baseMod
    USE typeMod

    IMPLICIT NONE

    TYPE base(l1,k1,l2)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1
        INTEGER, LEN :: l2

        CHARACTER(l1) :: label
        TYPE(baseType(k1,l2)), POINTER :: bT( : )

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual
            PROCEDURE, PASS :: NotEqual => BaseNotEqual

    END TYPE base

    CONTAINS

        LOGICAL FUNCTION BaseNotEqual(this, o)
            CLASS(base(*,8,*)), INTENT(in) :: this
            CLASS(base(*,8,*)), INTENT(in) :: o


            BaseNotEqual = .TRUE.
            IF ((this%l1 == o%l1)       .AND.&
                (this%l2 == o%l2)       .AND.&
                (this%label == o%label) .AND.&
                (.NOT. ANY(this%bT /= o%bT))) THEN
                BaseNotEqual = .FALSE.
            END IF

        END FUNCTION BaseNotEqual

END MODULE baseMod


PROGRAM aceCompPtrFunctionReturn01
    USE baseMod

    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: j
    INTEGER :: k

    INTEGER(8) :: int8Array( 27 ) = [ (INT(i, 8), i = 1, 27) ]

    CHARACTER(3), PARAMETER :: items( 3 ) = [ 'I-1', 'I-2', 'I-3' ]

    TYPE(baseType(8,3)), TARGET :: bT( 3,3 )
    TYPE(base(:,8,:)), POINTER :: pB( : )


    DO i = 1, 3
        DO j = 1, 3
            k = ((i - 1) * 3) + j
            k = ((k - 1) * 3) + 1

            bT( j,i )%a = int8Array( k:(k + 2) )
        END DO
    END DO


    pB => BuildArray( )

    DO i = 1, 3
        IF (pB( i )%label /= items( i ))    CALL zzrc( (10_4 + INT(i, 4)) )
        IF ( ANY(pB( i )%bT /= bT( :,i )) ) CALL zzrc( (20_4 + INT(i, 4)) )
    END DO


    CONTAINS

        FUNCTION BuildArray( )
            TYPE(base(:,8,:)), POINTER :: BuildArray( : )

            ALLOCATE(base(3,8,bT%l1) :: BuildArray( 3 ))

            DO i = 1, 3
                BuildArray( i )%label = items( i )
                BuildArray( i )%bT => bT( :,i )
            END DO

        END FUNCTION BuildArray

END PROGRAM aceCompPtrFunctionReturn01
