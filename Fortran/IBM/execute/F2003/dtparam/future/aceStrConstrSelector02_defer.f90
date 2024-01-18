!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : aceStrConstrSelector02
!*                               with DTP
!*
!*  DATE                       : November 13, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor contains Structure
!*                               Constructors (with Type Parameters)
!*  SECONDARY FUNCTIONS TESTED : and the Array Constructor is used as the
!*                               selector in an ASSOCIATE Construct
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
!*        o Structure Constructors,
!*
!*  2)  Testing the usage of an Array Constructor in various contexts:
!*      * As the selector of an ASSOCIATE Construct
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE bMod

    IMPLICIT NONE

    TYPE b(l1,k1)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1

        REAL(k1) :: array( l1 )

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual16
            PROCEDURE, PASS :: NotEqual16 => b16NotEqual

    END TYPE b

    CONTAINS

        LOGICAL FUNCTION b16NotEqual(this, o)
            CLASS(b(*,16)), INTENT(in) :: this
            CLASS(b(*,16)), INTENT(in) :: o


            b16NotEqual = .TRUE.
            IF ((this%k1 == o%k1)       .AND.&
                (this%l1 == o%l1)       .AND.&
                ( ALL(this%array == o%array) )) THEN
                b16NotEqual = .FALSE.
            END IF

        END FUNCTION b16NotEqual

END MODULE bMod


PROGRAM aceStrConstrSelector02
    USE bMod

    IMPLICIT NONE

    TYPE c(k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        REAL(k1) :: rA( (l1 + 1) )
        TYPE(b((l1 * 2),k1)) :: bA
    END TYPE c


    INTEGER :: i
    INTEGER :: j

    INTEGER(4) :: rc = 10_4

    REAL(16) :: rA( 6 ) = [ ((1.0_16 / REAL(i, 16)), i = 1, 6) ]

    TYPE(b(:,16)), ALLOCATABLE :: bA( : )


    DO i = 1, 3
        ALLOCATE(b((i * 2),16) :: bA( i ))

        DO j = 1, (i * 2)
            bA( j )%array = rA( :i )
        END DO

        CALL ConstructContainerSelectorArray(i, bA, rc)

        rc = rc + 20_4
        DEALLOCATE( bA )
    END DO


    CONTAINS

        SUBROUTINE ConstructContainerSelectorArray(l, a, rc)
            INTEGER :: l
            TYPE(b(*,16)) :: a( : )
            INTEGER(4) :: rc

            INTEGER :: i


            ASSOCIATE(array =>&
                [   c(16,l)(rA( :(l + 1) ),b((l * 2),16)(rA( :(l * 2) ))),&
                    c(16,l)(rA( :(l + 1) ),b((l * 2),16)(rA( :(l * 2) ))),&
                    c(16,l)(rA( :(l + 1) ),b((l * 2),16)(rA( :(l * 2) )))   ])

                IF (SIZE( array ) /= 3)         CALL zzrc( rc )

                DO i = 1, SIZE( array )
                    IF (SIZE( array( i )%rA ) /= (l + 1))&
                                        CALL zzrc( (rc + INT(i, 4)) )
                    IF ( ANY(array( i )%rA /= rA( :(l + 1) )) )&
                                        CALL zzrc( (rc + 5_4 + INT(i, 4)) )

                    IF ( ANY(array( i )%bA /= a) )     &
                                        CALL zzrc( (rc + 10_4 + INT(i, 4)) )
                END DO

            END ASSOCIATE

        END SUBROUTINE ConstructContainerSelectorArray

END PROGRAM aceStrConstrSelector02
