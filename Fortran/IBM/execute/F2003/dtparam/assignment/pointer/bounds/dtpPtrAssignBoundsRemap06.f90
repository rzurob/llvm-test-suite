!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : dtpPtrAssignBoundsRemap06
!*  TEST CASE TITLE            : POINTER Assignment (with Bounds Specification
!*                               and Remapping)
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : March 17, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Bounds Remapping for an Array instance
!*                               of an Object Hierarchy to a Rank 7 Array
!*  SECONDARY FUNCTIONS TESTED : Where each Dimension has an Extent of 7
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE mGrandParent
    IMPLICIT NONE

    TYPE tGrandParent(l1)
        INTEGER, LEN :: l1

        CHARACTER(l1) :: name

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual
            PROCEDURE, PASS :: NotEqual => GrandParentNotEqual

    END TYPE tGrandParent

    CONTAINS

        LOGICAL FUNCTION GrandParentNotEqual(this, o)
            CLASS(tGrandParent(*)), INTENT(in) :: this
            CLASS(tGrandParent(*)), INTENT(in) :: o


            GrandParentNotEqual = .FALSE.
            IF (this%name /= o%name) THEN
                GrandParentNotEqual = .TRUE.
            END IF

        END FUNCTION GrandParentNotEqual

END MODULE mGrandParent


MODULE mParent
    USE mGrandParent

    IMPLICIT NONE

    TYPE, EXTENDS(tGrandParent) :: tParent(k2)
        INTEGER, KIND :: k2

        INTEGER(k2) :: id

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual
            PROCEDURE, PASS :: NotEqual => Parent8NotEqual

    END TYPE tParent

    CONTAINS

        LOGICAL FUNCTION Parent8NotEqual(this, o)
            CLASS(tParent(*,8)), INTENT(in) :: this
            CLASS(tGrandParent(*)), INTENT(in) :: o


            Parent8NotEqual = .TRUE.
            SELECT TYPE (o)
                TYPE IS (tParent(*,8))
                    IF (.NOT. (this%tGrandParent /= o%tGrandParent)) THEN
                        IF (this%id == o%id) THEN
                            Parent8NotEqual = .FALSE.
                        END IF
                    END IF

                CLASS DEFAULT
                    PRINT *, "Parent8NotEqual():  Unknown Type"
            END SELECT

        END FUNCTION Parent8NotEqual

END MODULE mParent


MODULE mChild
    USE mParent

    IMPLICIT NONE

    TYPE, EXTENDS(tParent) :: tChild(k3,l3)
        INTEGER, KIND :: k3
        INTEGER, LEN :: l3

        REAL(k3) :: extendedData( l3 )

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual
            PROCEDURE, PASS :: NotEqual => Child_8_4_NotEqual

    END TYPE tChild

    CONTAINS

        LOGICAL FUNCTION Child_8_4_NotEqual(this, o)
            CLASS(tChild(*,8,4,*)), INTENT(in) :: this
            CLASS(tGrandParent(*)), INTENT(in) :: o


            Child_8_4_NotEqual = .TRUE.
            SELECT TYPE (o)
                TYPE IS (tChild(*,8,4,*))
                    IF (.NOT. (this%tParent /= o%tParent)) THEN
                        IF ( ALL(this%extendedData == o%extendedData) ) THEN
                            Child_8_4_NotEqual = .FALSE.
                        END IF
                    END IF

                CLASS DEFAULT
                    PRINT *, "Child_8_4_NotEqual():  Unknown Type"
            END SELECT

        END FUNCTION Child_8_4_NotEqual

END MODULE mChild


PROGRAM dtpPtrAssignBoundsRemap06
    USE mChild

    IMPLICIT NONE

    INTERFACE
        SUBROUTINE Pointer7X7(target, rc)
            USE mChild

            IMPLICIT NONE

            TYPE(tChild(*,8,4,*)), TARGET :: target( : )
            INTEGER(4) :: rc
        END SUBROUTINE Pointer7X7
    END INTERFACE

    INTEGER(8) :: i

    TYPE(tChild(6,8,4,1)), TARGET :: childTarget( 823543 )


    DO i = 1_8, 823543_8
        WRITE(childTarget( i )%name, '(I6.6)') i
        childTarget( i )%id = i
        childTarget( i )%extendedData = [ (1.0_4 / REAL(i, 4)) ]
    END DO

    CALL Pointer7X7(childTarget, 10_4)

END PROGRAM dtpPtrAssignBoundsRemap06


SUBROUTINE Pointer7X7(target, rc)
    USE mChild

    IMPLICIT NONE

    TYPE(tChild(*,8,4,*)), TARGET :: target( : )
    INTEGER(4) :: rc

    INTEGER :: i, j, k, l, m, n, o
    INTEGER(8) :: p
    TYPE(tChild(6,8,4,1)), POINTER :: ptr7X7( :,:,:,:,:,:,: )


    ptr7X7( 0:6,0:6,0:6,0:6,0:6,0:6,0:6 ) => target


    IF (.NOT. ASSOCIATED( ptr7X7 ))         CALL zzrc( rc )
    IF ( ANY(SHAPE( ptr7X7 ) /= [ 7, 7, 7, 7, 7, 7, 7 ]) )&
                                            CALL zzrc( (rc + 1_4) )


    DO i = 1, 7
        IF (SIZE(ptr7X7, i) /= 7)           CALL zzrc( (rc + 10_4 + INT(i, 4)) )
        IF (LBOUND(ptr7X7, i) /= 0)         CALL zzrc( (rc + 20_4 + INT(i, 4)) )
        IF (UBOUND(ptr7X7, i) /= 6)         CALL zzrc( (rc + 30_4 + INT(i, 4)) )
    END DO


    p = 0_8
    DO o = 0, 6
        DO n = 0, 6
            DO m = 0, 6
                DO l = 0, 6
                    DO k = 0, 6
                        DO j = 0, 6
                            DO i = 0, 6
                                p = p + 1_8
                                IF (ptr7X7( i,j,k,l,m,n,o ) /=&
                                                    target( p )) THEN
                                    PRINT *, "ptr7X7(", i, ",", j, ",", k,&
                                            ",", l, ",", m, ",", n, ",", o,&
                                            ") /= target(", p, ")"
                                    CALL zzrc( (rc + 100_4 + INT(p, 4)) )
                                END IF
                            END DO
                        END DO
                    END DO
                END DO
            END DO
        END DO
    END DO

END SUBROUTINE Pointer7X7
