!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : componentSelector03
!*  TEST CASE TITLE            : selector is a Component of a Derived Type
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : September  5, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : selector is the Component of a variable
!*                               of Dervied Type
!*  SECONDARY FUNCTIONS TESTED : The Component is a POINTER
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Testing where the selector is a Component of a Derived Type that uses
!*  Type Parameters, and the Component:
!*  * Has the POINTER Attribute
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE tElementMod
    IMPLICIT NONE

    TYPE tElement(k)
        INTEGER, KIND :: k

        REAL(k), POINTER :: value => NULL( )
    END TYPE tElement

END MODULE tElementMod


MODULE tTreeMod
    USE tElementMod

    IMPLICIT NONE

    TYPE, EXTENDS(tElement) :: tTree(l)
        INTEGER, LEN :: l

        TYPE(tTree(k,l)), POINTER :: left => NULL( )
        TYPE(tTree(k,l)), POINTER :: right => NULL( )

        CONTAINS

            PROCEDURE, NOPASS :: BuildNode16
            PROCEDURE, NOPASS :: FindValue16
            PROCEDURE, NOPASS :: SortTree16

            PROCEDURE, PASS :: AddNode16

            GENERIC :: BuildNode => BuildNode16
            GENERIC :: FindValue => FindValue16
            GENERIC :: SortTree => SortTree16

            GENERIC :: AddNode => AddNode16

    END TYPE tTree

    CONTAINS

        FUNCTION BuildNode16( value )
            REAL(16), TARGET :: value
            TYPE(tTree(16,:)), POINTER :: BuildNode16


            ALLOCATE(BuildNode16, SOURCE=tTree(16,2)(value))

        END FUNCTION BuildNode16


        RECURSIVE FUNCTION FindValue16(root, value)
            TYPE(tTree(16,:)), POINTER :: root
            REAL(16), TARGET :: value

            TYPE(tTree(16,:)), POINTER :: FindValue16


            FindValue16 => root
            IF ( ASSOCIATED( root ) ) THEN

                ASSOCIATE(val => root%value)
                    IF (val > value) THEN
                        FindValue16 => root%FindValue(root%left, value)

                    ELSE IF (val < value) THEN
                        FindValue16 => root%FindValue(root%right, value)
                    END IF
                END ASSOCIATE

                IF (.NOT. ASSOCIATED( FindValue16 )) THEN
                    FindValue16 => root
                END IF
            END IF

        END FUNCTION FindValue16


        RECURSIVE SUBROUTINE SortTree16(root, sortedList, idx)
            TYPE(tTree(16,*)), INTENT(in) :: root
            REAL(16), INTENT(inout) :: sortedList( : )
            INTEGER, INTENT(inout) :: idx


            IF ( ASSOCIATED( root%right ) ) THEN
                ASSOCIATE(right => root%right)
                    CALL root%SortTree(right, sortedList, idx)
                END ASSOCIATE
            END IF

            ASSOCIATE(value => root%value)
                sortedList( idx ) = value
            END ASSOCIATE
            idx = idx + 1

            IF ( ASSOCIATED( root%left ) ) THEN
                ASSOCIATE(left => root%left)
                    CALL root%SortTree(left, sortedList, idx)
                END ASSOCIATE
            END IF

        END SUBROUTINE SortTree16


        SUBROUTINE AddNode16(root, value)
            CLASS(tTree(16,*)), TARGET, INTENT(inout) :: root
            REAL(16), TARGET, INTENT(in) :: value

            TYPE(tTree(16,:)), POINTER :: node


            node => root
            node => root%FindValue16(node, value)

            ASSOCIATE(val => node%value)
                IF (val > value) THEN
                    node%left => root%BuildNode( value )

                ELSE IF (val < value) THEN
                    node%right => root%BuildNode( value )
                END IF
            END ASSOCIATE

        END SUBROUTINE AddNode16

END MODULE tTreeMod


PROGRAM componentSelector03
    USE tTreeMod

    IMPLICIT NONE

    INTEGER, PARAMETER :: N = 100

    REAL :: selector( N )
    LOGICAL :: selected( N ) = .FALSE.

    INTEGER(4) :: i
    INTEGER :: j

    REAL(16), TARGET :: sortedArray( N ) = [ ((1.0_16 / i), i = 1, N) ]
    REAL(16), TARGET :: randomArray( N )

    REAL(16) :: sortArray( N )

    TYPE(tTree(16,2)), POINTER :: root => NULL( )


    CALL RANDOM_NUMBER( selector )

    DO i = 1, N
        j = MOD(INT( (selector( i ) * N) ), N) + 1

        !
        !  Simplification:  If the current element has already been selected,
        !  use the next unselected element.  (This *should* eliminate the
        !  duplication of entries -- without causing infinite loops to occur.)
        !
        DO WHILE ( selected( j ) )
            j = MOD(j, N) + 1
        END DO

        selected( j ) = .TRUE.
        randomArray( i ) = sortedArray( j )
    END DO
    

    root => root%BuildNode( randomArray( 1 ) )
    DO i = 2, N
        CALL root%AddNode( randomArray( i ) )
    END DO


    j = 1
    CALL root%SortTree(root, sortArray, j)


    DO i = 1, N
        PRINT "(I3,F7.4,F7.4)", i, sortArray( i ), sortedArray( i )
        IF (sortArray( i ) /= sortedArray( i )) THEN
            CALL zzrc( (100_4 + i) )
        END IF
    END DO

END PROGRAM componentSelector03
