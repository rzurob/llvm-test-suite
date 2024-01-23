!***********************************************************************
!* =====================================================================
!*
!*                               with DTP
!*
!*  DATE                       : November 25, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor contains a Derived Type
!*                               with a Polymorphic POINTER Component
!*  SECONDARY FUNCTIONS TESTED : and is the source-expr in an ALLOCATE
!*                               Statement
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  1)  Testing the format of the Array Constructor:
!*      * Brackets:  (/ /)
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

    TYPE, ABSTRACT :: baseType(k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        CONTAINS

            GENERIC :: OPERATOR(/=) => TypeNotEqual
            PROCEDURE(TypeNotEqual), DEFERRED :: TypeNotEqual

    END TYPE baseType

    ABSTRACT INTERFACE
        ELEMENTAL LOGICAL FUNCTION TypeNotEqual(this, o)
            IMPORT baseType
            CLASS(baseType(8,*)), INTENT(in) :: this
            CLASS(baseType(8,*)), INTENT(in) :: o
        END FUNCTION TypeNotEqual
    END INTERFACE

    TYPE, EXTENDS(baseType) :: iType
        INTEGER(k1) :: i( l1 )

        CONTAINS

            PROCEDURE, PASS :: TypeNotEqual => ITypeNotEqual

    END TYPE iType

    TYPE, EXTENDS(baseType) :: rType
        REAL(k1) :: r( l1 )

        CONTAINS

            PROCEDURE, PASS :: TypeNotEqual => RTypeNotEqual

    END TYPE rType

    TYPE, EXTENDS(baseType) :: cType
        COMPLEX(k1) :: c( l1 )

        CONTAINS

            PROCEDURE, PASS :: TypeNotEqual => CTypeNotEqual

    END TYPE cType

    CONTAINS

        ELEMENTAL LOGICAL FUNCTION ITypeNotEqual(this, o)
            CLASS(iType(8,*)), INTENT(in) :: this
            CLASS(baseType(8,*)), INTENT(in) :: o


            ITypeNotEqual = .TRUE.
            SELECT TYPE (o)
                TYPE IS (iType(8,*))
                    IF ((this%l1 == o%l1)   .AND.&
                        ( ALL(this%i == o%i) )) THEN
                        ITypeNotEqual = .FALSE.
                    END IF
            END SELECT

        END FUNCTION ITypeNotEqual

        ELEMENTAL LOGICAL FUNCTION RTypeNotEqual(this, o)
            CLASS(rType(8,*)), INTENT(in) :: this
            CLASS(baseType(8,*)), INTENT(in) :: o


            RTypeNotEqual = .TRUE.
            SELECT TYPE (o)
                TYPE IS (rType(8,*))
                    IF ((this%l1 == o%l1)   .AND.&
                        ( ALL(this%r == o%r) )) THEN
                        RTypeNotEqual = .FALSE.
                    END IF
            END SELECT

        END FUNCTION RTypeNotEqual

        ELEMENTAL LOGICAL FUNCTION CTypeNotEqual(this, o)
            CLASS(cType(8,*)), INTENT(in) :: this
            CLASS(baseType(8,*)), INTENT(in) :: o


            CTypeNotEqual = .TRUE.
            SELECT TYPE (o)
                TYPE IS (cType(8,*))
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

    TYPE base(l1,k1)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1

        CLASS(baseType(k1,l1)), POINTER :: bT

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual
            PROCEDURE, PASS :: NotEqual => BaseNotEqual

    END TYPE base

    CONTAINS

        ELEMENTAL LOGICAL FUNCTION BaseNotEqual(this, o)
            CLASS(base(*,8)), INTENT(in) :: this
            CLASS(base(*,8)), INTENT(in) :: o


            BaseNotEqual = .TRUE.
            IF ((this%l1 == o%l1)           .AND.&
                ( ASSOCIATED( o%bT ) )      .AND.&
                ( ASSOCIATED( this%bT ) )) THEN
                IF (.NOT. (this%bT /= o%bT)) BaseNotEqual = .FALSE.
            END IF

        END FUNCTION BaseNotEqual

END MODULE baseMod


PROGRAM aceCompPtrAllocSourceSpec01
    USE baseMod

    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: stat

    CHARACTER(255) :: errmsg

    INTEGER(8), PARAMETER   :: i8A( 3 ) = (/ (INT(i, 8), i = 1, 3) /)
    REAL(8), PARAMETER      :: r8A( 6 ) = (/ ((1.0_8 / REAL(i, 8)), i = 1, 6) /)
    COMPLEX(8), PARAMETER   :: c8A( 3 ) =&
                        (/ (CMPLX(r8A( i ), r8A( (i + 1) ), 8), i = 1, 6, 2) /)

    TYPE(iType(8,3)), TARGET :: iT8 = iType(8,3)(i8A)
    TYPE(rType(8,3)), TARGET :: rT8 = rType(8,3)(r8A( 1:3 ))
    TYPE(cType(8,3)), TARGET :: cT8 = cType(8,3)(c8A)

    TYPE(base(3,8)), TARGET :: b1, b2, b3

    TYPE(base(:,8)), ALLOCATABLE :: bA( : )


    b1%bT => iT8
    b2%bT => rT8
    b3%bT => cT8

    ALLOCATE(bA( 3 ), SOURCE=(/ b1, b2, b3 /), STAT=stat, ERRMSG=errmsg)
    IF (stat /= 0) THEN
        PRINT *, 'ALLOCATE(STAT=', stat, '):', errmsg
        ERROR STOP 10_4
    END IF

    CALL VerifyBA( 20_4 )

    DEALLOCATE( bA )


    CALL BuildBA1(iT8, rT8, cT8, 30_4)
    DEALLOCATE( bA )

    CALL BuildBA2(b1, b2, b3, 70_4)
    DEALLOCATE( bA )


    CONTAINS

        SUBROUTINE BuildBA1(aiT8, arT8, acT8, rc)
            TYPE(iType(8,*)), TARGET :: aiT8
            TYPE(rType(8,*)), TARGET :: arT8
            TYPE(cType(8,*)), TARGET :: acT8
            INTEGER(4) :: rc


            TYPE(base(:,8)), POINTER :: lb1, lb2, lb3


            ALLOCATE(base(3,8) :: lb1)
            lb1%bT => iT8

            lb2 => b2

            ALLOCATE(base(3,8) :: lb3)
            lb3%bT => cT8

            ALLOCATE(bA( 3 ), SOURCE=(/ lb1, lb2, lb3 /),&
                                    STAT=stat, ERRMSG=errmsg)
            IF (stat /= 0) THEN
                PRINT *, 'ALLOCATE(STAT=', stat, '):', errmsg
                CALL zzrc( rc )
            END IF

            CALL VerifyBA( (rc + 10_4) )
            DEALLOCATE( bA )

            CALL BuildBA2(lb1, lb2, lb3, (rc + 20_4))

        END SUBROUTINE BuildBA1

        SUBROUTINE BuildBA2(aB1, aB2, aB3, rc)
            TYPE(base(*,8)), TARGET :: aB1
            TYPE(base(*,8)), TARGET :: aB2
            TYPE(base(*,8)), TARGET :: aB3
            INTEGER(4) :: rc


            ALLOCATE(bA( 3 ), SOURCE=(/ aB1, aB2, aB3 /),&
                                    STAT=stat, ERRMSG=errmsg)
            IF (stat /= 0) THEN
                PRINT *, 'ALLOCATE(STAT=', stat, '):', errmsg
                CALL zzrc( rc )
            END IF

            CALL VerifyBA( (rc + 10_4) )

        END SUBROUTINE BuildBA2

        SUBROUTINE VerifyBA( rc )
            INTEGER(4) :: rc

            IF (.NOT. ALLOCATED( bA )) THEN
                CALL zzrc( rc )

            ELSE IF ( ANY(SHAPE( bA ) /= (/ 3 /)) ) THEN
                CALL zzrc( (rc + 1_4) )

            ELSE IF ( ANY(bA /= (/ b1, b2, b3 /)) ) THEN
                CALL zzrc( (rc + 2_4) )

            ELSE IF (bA( 1 ) /= b1) THEN
                CALL zzrc( (rc + 3_4) )

            ELSE IF (bA( 2 ) /= b2) THEN
                CALL zzrc( (rc + 4_4) )

            ELSE IF (bA( 3 ) /= b3) THEN
                CALL zzrc( (rc + 5_4) )
            END IF

        END SUBROUTINE VerifyBA

END PROGRAM aceCompPtrAllocSourceSpec01
