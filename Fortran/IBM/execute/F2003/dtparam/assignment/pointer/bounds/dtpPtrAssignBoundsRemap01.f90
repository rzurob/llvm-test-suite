!***********************************************************************
!* =====================================================================
!*
!*                               and Remapping)
!*
!*  DATE                       : March  9, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : Bounds Remapping for an Array instance of
!*                               an Object Hierarchy to a 2-Dimension Array
!*  SECONDARY FUNCTIONS TESTED : Where data-target is Polymorphic, and
!*                               data-pointer-object is the same type (or
!*                               an ancestor type of) data-target
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
    CHARACTER(3), PARAMETER :: charData( 10 ) =&
                    [ ('E-' // CHAR( mgpi ), mgpi = 65, 74) ]

    CONTAINS

        LOGICAL FUNCTION GrandParentNotEqual(this, o)
            CLASS(tGrandParent(*)), INTENT(in) :: this
            CLASS(tGrandParent(*)), INTENT(in) :: o


            GrandParentNotEqual = .FALSE.
            IF (this%name /= o%name) THEN
                GrandParentNotEqual = .TRUE.
            END IF

        END FUNCTION GrandParentNotEqual


        FUNCTION GrandParentMung(this, u1, u2)
            CLASS(tGrandParent(*)), TARGET :: this( : )
            INTEGER :: u1
            INTEGER :: u2

            INTEGER :: mungSize
            TYPE(tGrandParent(:)), POINTER :: GrandParentMung( :,: )


            mungSize = u1 * u2
            IF (mungSize > SIZE( this )) THEN
                PRINT *, "GrandParentMung(", SIZE( this ), ",",&
                            u1, ",", u2, "): ", mungSize, " is too big"

            ELSE
                GrandParentMung( 1:u1,1:u2 ) => this
            END IF

        END FUNCTION GrandParentMung

END MODULE mGrandParent


MODULE mParent
    USE mGrandParent

    IMPLICIT NONE

    TYPE, EXTENDS(tGrandParent) :: tParent(k2)
        INTEGER, KIND :: k2

        INTEGER(k2) :: id = -999

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual
            PROCEDURE, PASS :: NotEqual => Parent8NotEqual

    END TYPE tParent


    INTEGER(8) :: mpi
    INTEGER(8), PARAMETER :: intData( 10 ) = [ (mpi, mpi = 101_8, 110_8) ]

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


        FUNCTION ParentMung8(this, u1, u2)
            CLASS(tParent(*,8)), TARGET :: this( : )
            INTEGER :: u1
            INTEGER :: u2

            INTEGER :: mungSize
            TYPE(tParent(:,8)), POINTER :: ParentMung8( :,: )


            mungSize = u1 * u2
            IF (mungSize > SIZE( this )) THEN
                PRINT *, "ParentMung8(", SIZE( this ), ",",&
                            u1, ",", u2, "): ", mungSize, " is too big"

            ELSE
                ParentMung8( 1:u1,1:u2 ) => this
            END IF

        END FUNCTION ParentMung8

END MODULE mParent


MODULE mChild
    USE mParent

    IMPLICIT NONE

    TYPE, EXTENDS(tParent) :: tChild(k3,l3)
        INTEGER, KIND :: k3
        INTEGER, LEN :: l3

        REAL(k3) :: extendedData( l3 ) = -1.0_8

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual
            PROCEDURE, PASS :: NotEqual => Child8NotEqual

    END TYPE tChild


    INTEGER :: mci

    REAL(8), PARAMETER :: real8Data( 50 ) =&
                [ (1.0_8 / REAL(mci, 8), mci = 1, 50) ]

    CONTAINS

        LOGICAL FUNCTION Child8NotEqual(this, o)
            CLASS(tChild(*,8,8,*)), INTENT(in) :: this
            CLASS(tGrandParent(*)), INTENT(in) :: o


            Child8NotEqual = .TRUE.
            SELECT TYPE (o)
                TYPE IS (tChild(*,8,8,*))
                    IF (.NOT. (this%tParent /= o%tParent)) THEN
                        IF ( ALL(this%extendedData == o%extendedData) ) THEN
                            Child8NotEqual = .FALSE.
                        END IF
                    END IF

                CLASS DEFAULT
                    PRINT *, "Child8NotEqual():  Unknown Type"
            END SELECT

        END FUNCTION Child8NotEqual


        FUNCTION ChildMung8(this, u1, u2)
            CLASS(tChild(*,8,8,*)), TARGET :: this( : )
            INTEGER :: u1
            INTEGER :: u2

            INTEGER :: mungSize
            TYPE(tChild(:,8,8,:)), POINTER :: ChildMung8( :,: )


            mungSize = u1 * u2
            IF (mungSize > SIZE( this )) THEN
                PRINT *, "ChildMung8(", SIZE( this ), ",",&
                            u1, ",", u2, "): ", mungSize, " is too big"

            ELSE
                ChildMung8( 1:u1,1:u2 ) => this
            END IF

        END FUNCTION ChildMung8

END MODULE mChild


PROGRAM dtpPtrAssignBoundsRemap01
    USE mChild

    IMPLICIT NONE


    INTEGER :: i
    INTEGER :: j
    INTEGER :: k
    INTEGER :: m

    INTEGER :: dim1
    INTEGER :: dim2

    INTEGER(4) :: baseRC

    TYPE(tGrandParent(:)),  POINTER :: grandParentPtr( :,: ) => NULL( )
    TYPE(tParent(:,8)),     POINTER :: parentPtr( :,: ) => NULL( )
    TYPE(tChild(:,8,8,:)),  POINTER :: childPtr( :,: ) => NULL( )

    TYPE(tChild(3,8,8,5)), TARGET :: childTarget( 10 ) =&
            [ (tChild(3,8,8,5)(charData( i ),intData( i ),&
                            [ (real8Data( j ), j = (((i - 1) * 5) + 1),&
                                        (((i - 1) * 5) + 5)) ]), i = 1, 10) ]


    DO dim1 = 1, 3
        DO dim2 = 1, 3
            baseRC = INT(((((dim1 - 1) * 3) + dim2) * 40), 4)

            GrandParentPtr => GrandParentMung(childTarget, dim1, dim2)
            parentPtr => ParentMung8(childTarget, dim1, dim2)
            childPtr => ChildMung8(childTarget, dim1, dim2)


            IF (.NOT. ASSOCIATED( grandParentPtr ))&
                                        CALL zzrc( baseRC )
            IF ( ANY(SHAPE( grandParentPtr ) /= [ dim1, dim2 ]) )&
                                        CALL zzrc( (baseRC + 1_4) )

            IF (.NOT. ASSOCIATED( parentPtr ))&
                                        CALL zzrc( (baseRC + 2_4) )
            IF ( ANY(SHAPE( parentPtr ) /= [ dim1, dim2 ]) )&
                                        CALL zzrc( (baseRC + 3_4) )

            IF (.NOT. ASSOCIATED( childPtr ))&
                                        CALL zzrc( (baseRC + 4_4) )
            IF ( ANY(SHAPE( childPtr ) /= [ dim1, dim2 ]) )&
                                        CALL zzrc( (baseRC + 5_4) )


            DO i = 1, dim1
                k = (i - 1) * dim1

                DO j = 1, dim2
                    m = k + j

                    IF (grandParentPtr( j,i ) /= childTarget( m )%tGrandParent)&
                                    CALL zzrc( (baseRC + 10_4 + INT(m, 4)) )
                    IF (parentPtr( j,i ) /= childTarget( m )%tParent)&
                                    CALL zzrc( (baseRC + 20_4 + INT(m, 4)) )
                    IF (childPtr( j,i ) /= childTarget( m ))&
                                    CALL zzrc( (baseRC + 30_4 + INT(m, 4)) )
                END DO
            END DO
        END DO
    END DO

END PROGRAM dtpPtrAssignBoundsRemap01
