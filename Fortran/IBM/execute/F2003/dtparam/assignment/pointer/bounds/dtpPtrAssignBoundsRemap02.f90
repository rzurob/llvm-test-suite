!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : dtpPtrAssignBoundsRemap02
!*                               and Remapping)
!*
!*  DATE                       : March 10, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Bounds Remapping for a non-Polymorphic
!*                               Array instance of an Object Hierarchy to
!*                               a non-Polymorphic 3-Dimension Array
!*  SECONDARY FUNCTIONS TESTED : data-target is a POINTER that is associated
!*                               with a TARGET
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATE
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

        CHARACTER(l1) :: name = CHAR( 27 )

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual
            PROCEDURE, PASS :: NotEqual => GrandParentNotEqual

    END TYPE tGrandParent


    INTEGER :: mgpi
    CHARACTER(3), PARAMETER :: charData( 27 ) =&
                    [ ('V-' // CHAR( mgpi ), mgpi = 65, 91) ]

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

        INTEGER(k2) :: id = INT(-999, k2)

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual
            PROCEDURE, PASS :: NotEqual => Parent2NotEqual

    END TYPE tParent


    INTEGER(2) :: mpi
    INTEGER(2), PARAMETER :: intData( 27 ) = [ (mpi, mpi = 101_2, 127_2) ]

    CONTAINS

        LOGICAL FUNCTION Parent2NotEqual(this, o)
            CLASS(tParent(*,2)), INTENT(in) :: this
            CLASS(tGrandParent(*)), INTENT(in) :: o


            Parent2NotEqual = .TRUE.
            SELECT TYPE (o)
                TYPE IS (tParent(*,2))
                    IF (.NOT. (this%tGrandParent /= o%tGrandParent)) THEN
                        IF (this%id == o%id) THEN
                            Parent2NotEqual = .FALSE.
                        END IF
                    END IF

                CLASS DEFAULT
                    PRINT *, "Parent2NotEqual():  Unknown Type"
            END SELECT

        END FUNCTION Parent2NotEqual

END MODULE mParent


MODULE mChild
    USE mParent

    IMPLICIT NONE

    TYPE, EXTENDS(tParent) :: tChild(k3,l3)
        INTEGER, KIND :: k3
        INTEGER, LEN :: l3

        REAL(k3) :: extendedData( l3 ) = REAL(-1.0, k3)

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual
            PROCEDURE, PASS :: NotEqual => Child2_16NotEqual

            GENERIC :: MungArray => MungChild
            PROCEDURE, NOPASS :: MungChild => Child2_16Mung

    END TYPE tChild


    INTEGER :: mci

    REAL(16), PARAMETER :: real16Data( 150 ) =&
                [ (1.0_16 / REAL(mci, 16), mci = 1, 150) ]

    CONTAINS

        LOGICAL FUNCTION Child2_16NotEqual(this, o)
            CLASS(tChild(*,2,16,*)), INTENT(in) :: this
            CLASS(tGrandParent(*)), INTENT(in) :: o


            Child2_16NotEqual = .TRUE.
            SELECT TYPE (o)
                TYPE IS (tChild(*,2,16,*))
                    IF (.NOT. (this%tParent /= o%tParent)) THEN
                        IF ( ALL(this%extendedData == o%extendedData) ) THEN
                            Child2_16NotEqual = .FALSE.
                        END IF
                    END IF

                CLASS DEFAULT
                    PRINT *, "Child2_16NotEqual():  Unknown Type"
            END SELECT

        END FUNCTION Child2_16NotEqual


        SUBROUTINE Child2_16Mung(this, that, ub1, ub2, ub3)
            TYPE(tChild(:,2,16,:)), POINTER, INTENT(in) :: this( : )
            TYPE(tChild(:,2,16,:)), POINTER, INTENT(out) :: that( :,:,: )
            INTEGER :: ub1
            INTEGER :: ub2
            INTEGER :: ub3

            INTEGER :: mungSize


            mungSize = ub1 * ub2 * ub3
            IF (mungSize > SIZE( this )) THEN
                PRINT *, "Child2_16Mung(", SIZE( this ), ",", ub1, ",",&
                            ub2, ",", ub3, "): ", mungSize, " is too big"

            ELSE
                that( 1:ub1,1:ub2,1:ub3 ) => this
            END IF

        END SUBROUTINE Child2_16Mung

END MODULE mChild


PROGRAM dtpPtrAssignBoundsRemap02
    USE mChild

    IMPLICIT NONE


    INTEGER :: i
    INTEGER :: j
    INTEGER :: k
    INTEGER :: m

    INTEGER :: dim1
    INTEGER :: dim2
    INTEGER :: dim3

    INTEGER(4) :: baseRC


    TYPE(tChild(3,2,16,5)), TARGET :: childTarget( 27 )

    TYPE(tChild(:,2,16,:)), POINTER :: cp( : )
    TYPE(tChild(:,2,16,:)), POINTER :: childPtr( :,:,: ) => NULL( )


    DO i = 1, 27
        childTarget( i )%id     = intData( i )
        childTarget( i )%name   = charData( i )

        j = ((i - 1) * 5) + 1
        childTarget( i )%extendedData = [ (real16Data( k ), k = j, (j + 4)) ]
    END DO


    cp => childTarget


    baseRC = 10_4
    DO dim1 = 1, 3
        DO dim2 = 1, 3
            DO dim3 = 1, 3
                CALL cp%MungArray(cp, childPtr, dim1, dim2, dim3)


                IF (.NOT. ASSOCIATED( childPtr ))&
                                            CALL zzrc( (baseRC + 1_4) )
                IF ( ANY(SHAPE( childPtr ) /= [ dim1, dim2, dim3 ]) )&
                                            CALL zzrc( (baseRC + 2_4) )


                m = 1
                DO k = 1, dim3
                    DO j = 1, dim2
                        DO i = 1, dim1
                            IF (childPtr( i,j,k ) /= childTarget( m ))&
                                    CALL zzrc( (baseRC + 2_4 + INT(m, 4)) )
                            m = m + 1
                        END DO
                    END DO
                END DO

                baseRC = baseRC + 30_4
            END DO
        END DO
    END DO

END PROGRAM dtpPtrAssignBoundsRemap02
