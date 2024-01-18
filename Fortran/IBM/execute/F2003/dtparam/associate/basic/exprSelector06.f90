!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : exprSelector06
!*  TEST CASE TITLE            : expr selector with Derived Type Parameters
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : July 18, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : selector is an Expression
!*  SECONDARY FUNCTIONS TESTED : The Expression is a Defined Operation
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Basic Testing where selector is:
!*  o  An expr that contains a:
!*     -  Defined Operation
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE setMod
    IMPLICIT NONE

    TYPE, ABSTRACT :: set(l,k)
        INTEGER, LEN :: l
        INTEGER, KIND :: k

        CONTAINS

            PROCEDURE(setUnion), DEFERRED  :: setUnion
            GENERIC :: OPERATOR(.union.) => setUnion

            PROCEDURE(setIntersection), DEFERRED :: setIntersection
            GENERIC :: OPERATOR(.intersection.) => setIntersection

    END TYPE set

    INTERFACE setUnion
        FUNCTION setUnion(set1, set2)
            IMPORT set
            CLASS(set(*,4)), INTENT(in) :: set1
            CLASS(set(*,4)), INTENT(in) :: set2

            CLASS(set(:,4)), ALLOCATABLE :: setUnion
        END FUNCTION setUnion
    END INTERFACE setUnion

    INTERFACE setIntersection
        FUNCTION setIntersection(set1, set2)
            IMPORT set
            CLASS(set(*,4)), INTENT(in) :: set1
            CLASS(set(*,4)), INTENT(in) :: set2

            CLASS(set(:,4)), ALLOCATABLE :: setIntersection
        END FUNCTION setIntersection
    END INTERFACE setIntersection

END MODULE setMod

MODULE intSetMod
    USE setMod
    IMPLICIT NONE

    TYPE, EXTENDS(set) :: intSet
        INTEGER(k) :: elem( l )

        CONTAINS

            PROCEDURE :: setUnion => int4SetUnion
            PROCEDURE :: setIntersection => int4SetIntersection

    END TYPE intSet

    CONTAINS

! Standard! If Polymorphic arguments is an F2003 Standard requirement,
!           Standard is broken -- this is an instantiation not an
!           Abstract Interface!
        FUNCTION int4SetUnion(set1, set2)
            CLASS(intSet(*,4)), INTENT(in) :: set1
            CLASS(set(*,4)), INTENT(in) :: set2

            CLASS(set(:,4)), ALLOCATABLE :: int4SetUnion

            INTEGER :: i = 1
            INTEGER :: j = 1
            INTEGER :: k

            INTEGER :: total
            INTEGER(4), ALLOCATABLE :: setArray( : )

            SELECT TYPE ( set1 )
                TYPE IS (intSet(*,4))
                    SELECT TYPE ( set2 )
                        TYPE IS (intSet(*,4))
                            total = set1%l + set2%l&
                                    - COUNT(set1%elem == set2%elem)

                            ALLOCATE(setArray( total ))

                            !
                            !  Assumes both sets are sorted.
                            !
                            DO k = 1, total
                                IF ((i <= set1%l) .AND.&
                                    (j <= set2%l)) THEN
                                    IF (set1%elem( i ) < set2%elem( j )) THEN
                                        setArray( k ) = set1%elem( i )
                                        i = i + 1

                                    ELSE&
                                      IF (set1%elem( i ) > set2%elem( j )) THEN
                                        setArray( k ) = set2%elem( j )
                                        j = j + 1
                                    ELSE
                                        setArray( k ) = set1%elem( i )
                                        i = i + 1
                                        j = j + 1
                                    END IF

                                ELSE IF (i <= set1%l) THEN
                                    setArray( k ) = set1%elem( i )
                                    i = i + 1

                                ELSE
                                    setArray( k ) = set2%elem( j )
                                    j = j + 1
                                END IF
                            END DO
                        CLASS DEFAULT
                            STOP 10
                    END SELECT

                CLASS DEFAULT
                    STOP 11
            END SELECT

            ALLOCATE(int4SetUnion, SOURCE=intSet(total,4)(setArray))

            SELECT TYPE ( int4SetUnion )
                TYPE IS (intSet(*,4))
PRINT *, int4SetUnion%elem
            END SELECT

PRINT *, setArray
            DEALLOCATE( setArray )

        END FUNCTION int4SetUnion

        FUNCTION int4SetIntersection(set1, set2)
            CLASS(intSet(*,4)), INTENT(in) :: set1
            CLASS(set(*,4)), INTENT(in) :: set2

            CLASS(set(:,4)), ALLOCATABLE :: int4SetIntersection

            INTEGER :: i = 1
            INTEGER :: j = 1
            INTEGER :: k = 1
            INTEGER :: l

            INTEGER :: total
            INTEGER(4), ALLOCATABLE :: setArray( : )

            SELECT TYPE ( set1 )
                TYPE IS (intSet(*,4))
                    SELECT TYPE ( set2 )
                        TYPE IS (intSet(*,4))
                            total = COUNT(set1%elem == set2%elem)
                            ALLOCATE(setArray( total ))

                            DO l = 1, set1%l
                                IF ((i <= set1%l) .AND.&
                                    (j <= set2%l)) THEN
                                    IF (set1%elem( i ) == set2%elem( j )) THEN
                                        setArray( k ) = set1%elem( i )

                                        i = i + 1
                                        j = j + 1
                                        k = k + 1

                                    ELSE
                                        DO WHILE ((i < set1%l) .AND.&
                                            (set1%elem( i ) < set2%elem( j )))
                                            i = i + 1
                                        END DO

                                        DO WHILE ((j < set2%l) .AND.&
                                            (set2%elem( j ) < set1%elem( i )))
                                            j = j + 1
                                        END DO
                                    END IF
                                END IF
                            END DO

                        CLASS DEFAULT
                            STOP 20
                    END SELECT

                CLASS DEFAULT
                    STOP 21
            END SELECT

            ALLOCATE(int4SetIntersection, SOURCE=intSet(total,4)(setArray))

            SELECT TYPE ( int4SetIntersection )
                TYPE IS (intSet(*,4))
PRINT *, int4SetIntersection%elem
            END SELECT

PRINT *, setArray
            DEALLOCATE( setArray )

        END FUNCTION int4SetIntersection

END MODULE intSetMod


PROGRAM exprSelector06
    USE intSetMod
    IMPLICIT NONE

    ! DEFECT! SIGSEGV if Polymorphic Types
    TYPE(intSet(3,4)) :: setA = intSet(3,4)([ 1, 3, 5 ])
    TYPE(intSet(5,4)) :: setB = intSet(5,4)([ 1, 4, 5, 7, 9 ])

    INTEGER(4) :: unionSet( 6 ) = [ 1, 3, 4, 5, 7, 9 ]
    INTEGER(4) :: intersectionSet( 2 ) = [ 1, 5 ]
    INTEGER(4) :: complexExprSet( 3 ) = [ 1, 3, 5 ]


    PRINT *, 'Union:'

    ASSOCIATE(theUnion => setA .union. setB)
        SELECT TYPE ( theUnion )
            TYPE IS (intSet(*,4))
                PRINT *, SIZE(theUnion%elem)
                PRINT *, KIND(theUnion%elem)
                PRINT *, theUnion%elem

                IF ( ANY(theUnion%elem /= unionSet) ) THEN
                    STOP 30
                END IF

            CLASS DEFAULT
                STOP 31
        END SELECT
    END ASSOCIATE


    PRINT *
    PRINT *, 'Intersection:'

    ASSOCIATE(theIntersection => setA .intersection. setB)
        SELECT TYPE ( theIntersection )
            TYPE IS (intSet(*,4))
                PRINT *, SIZE(theIntersection%elem)
                PRINT *, KIND(theIntersection%elem)
                PRINT *, theIntersection%elem

                IF ( ANY(theIntersection%elem /= intersectionSet) ) THEN
                    STOP 40
                END IF

            CLASS DEFAULT
                STOP 41
        END SELECT
    END ASSOCIATE


    PRINT *
    PRINT *, 'Complex Defined Operation Expression:'

    ASSOCIATE(aComplexExpr => (setA .union. setB) .intersection. setA)
        SELECT TYPE ( aComplexExpr )
            TYPE IS (intSet(*,4))
                PRINT *, SIZE(aComplexExpr%elem)
                PRINT *, KIND(aComplexExpr%elem)
                PRINT *, aComplexExpr%elem

                IF ( ANY(aComplexExpr%elem /= complexExprSet) ) THEN
                    STOP 50
                END IF

            CLASS DEFAULT
                STOP 51
        END SELECT
    END ASSOCIATE

END PROGRAM exprSelector06
