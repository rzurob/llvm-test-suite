!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : componentSelector05
!*
!*  DATE                       : September 12, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : selector is the Component of a variable
!*                               of Dervied Type
!*  SECONDARY FUNCTIONS TESTED : The Component is a Polymorphic POINTER
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Testing where the selector is a Component of a Derived Type that uses
!*  Type Parameters, and the Component:
!*  * Is a Polymorphic POINTER
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE tBaseMod
    IMPLICIT NONE

    TYPE tBase(l)
        INTEGER, LEN :: l

        CHARACTER(l) :: compType
        CLASS(tBase(:)), POINTER :: compBase
    END TYPE tBase

END MODULE tBaseMod


MODULE tDerivedMod
    USE tBaseMod

    IMPLICIT NONE

    TYPE, EXTENDS(tBase) :: tDerived(k)
        INTEGER, KIND :: k

        LOGICAL(k) :: compAlloc
        CLASS(tBase(:)), POINTER :: compDerived( : )
    END TYPE tDerived

END MODULE tDerivedMod


PROGRAM componentSelector05
    USE tDerivedMod

    IMPLICIT NONE

    INTEGER, PARAMETER :: N = 3

    INTEGER(4) :: i = 1_4

    CHARACTER(2) :: char2Str = 'zY'
    CHARACTER(3) :: char3Str = 'IBM'

    TYPE(tBase(1)), TARGET :: basePoly( N )
    TYPE(tBase(2)), TARGET :: base

    CLASS(tBase(:)), POINTER :: derivedPoly


    !
    !  Build a Linked List:  C -> B -> A -> NULL()
    !  As Array Elements:  basePoly(1) == A, 2 == B, 3 == C
    !
    DO i = 1, N
        basePoly( i ) = tBase(1)(ACHAR(64 + i),NULL())
        IF (i > 1) THEN
            basePoly( i )%compBase => basePoly(i - 1)
        END IF
    END DO


    !
    !  Check the array portion of the Linked List conforms to the above
    !  for 1, 1:2, and 1:3.  Also trace the Linked List.
    !
    DO i = 1, N
        PRINT *
        PRINT *, 'main()  CheckArray( basePoly( 1:', i, ') )'
        CALL CheckArray(basePoly( 1:i ), ((i - 1) * 20_4))

        PRINT *
        PRINT *, 'main()  WalkList( basePoly(', i, ') )'
        CALL WalkList(basePoly( i ), ((i - 1) * 150_4))
    END DO


    !
    !  Add "base" to the end of this List:  zY -> C -> B -> A -> NULL()
    !
    base = tBase(2)(char2Str,basePoly( N ))


    PRINT *
    PRINT *, 'main()  WalkList( base )'
    CALL WalkList(base, 600_4)


    ALLOCATE( tDerived(3,8)::derivedPoly )

    SELECT TYPE ( derivedPoly )
        TYPE IS (tDerived(*,8))
            !
            !  Add "derivedPoly%compBase" to the end of the Linked List
            !  (above):  IBM -> zY -> C -> B -> A -> NULL()
            !
            derivedPoly%compType = char3Str
            derivedPoly%compBase => base

            !
            !  Reference the original array from:  "derivedPoly%compDerived".
            !
            derivedPoly%compAlloc = .TRUE.
            derivedPoly%compDerived => basePoly

            PRINT *
            PRINT *, 'main()  WalkList( derivedPoly%compBase )'
            CALL WalkList(derivedPoly%compBase, 750_4)

            PRINT *
            PRINT *, 'main()  WalkList( derivedPoly%tBase )'
            CALL WalkList(derivedPoly%tBase, 900_4)

            DO i = 1, N
                PRINT *
                PRINT *,&
                    'main()  CheckArray( derivedPoly%compDerived( 1:', i, ') )'
                CALL CheckArray(derivedPoly%compDerived( 1:i ),&
                                    (((i - 1) * 20_4) + 1000_4))

                PRINT *
                PRINT *, 'main()  WalkList( derivedPoly%compDerived(', i, ') )'
                CALL WalkList(derivedPoly%compDerived( i ),&
                                    (((i - 1) * 150_4) + 1000_4))
            END DO

        CLASS DEFAULT
            CALL zzrc( 1600_4 )
    END SELECT


    CONTAINS

        SUBROUTINE CheckArray(array, rc)
            TYPE(tBase(*)) :: array(:)
            INTEGER(4) :: rc

            INTEGER(4) :: i
            INTEGER(4) :: j

            CHARACTER(1) :: str = ACHAR(65)


            PRINT *, 'CheckArray(', array( 1 )%compType, ',', rc, ')'
            DO i = SIZE( array ), 1, -1
                j = i * 5_4 + rc

                PRINT *, 'CheckArray() ', i, j, 'str = (', str, ')'

                SELECT CASE ( i )
                    CASE( 3 )
                        ASSOCIATE(b => array( i )%compBase%compBase)
                            CALL VerifyBase(b, 1, str, .FALSE., j)
                        END ASSOCIATE

                    CASE( 2 )
                        ASSOCIATE(b => array( i )%compBase)
                            CALL VerifyBase(b, 1, str, .FALSE., j)
                        END ASSOCIATE

                    CASE( 1 )
                        ASSOCIATE(b => array( i ))
                            CALL VerifyBase(b, 1, str, .FALSE., j)
                        END ASSOCIATE

                    CASE DEFAULT
                        CALL zzrc( rc )
                END SELECT
            END DO

        END SUBROUTINE CheckArray

        SUBROUTINE VerifyBase(base, l, str, assoc, rc)
            TYPE(tBase(*)) :: base
            INTEGER :: l
            CHARACTER(*) :: str
            LOGICAL :: assoc
            INTEGER(4) :: rc


            PRINT *, 'VerifyBase(', base%compType, ',', l,&
                            ',', str, ',', assoc, ',', rc, ')'

            IF (LEN( base%compType ) /= l) THEN
                CALL zzrc( rc )

            ELSE IF (base%compType /= str) THEN
                CALL zzrc( (rc + 1_4) )

            ELSE IF (ASSOCIATED( base%compBase ) .NEQV. assoc) THEN
                CALL zzrc( (rc + 2_4) )
            END IF

        END SUBROUTINE VerifyBase

        RECURSIVE SUBROUTINE WalkList(list, rc)
            TYPE(tBase(*)) :: list
            INTEGER(4) :: rc

            INTEGER :: m
            INTEGER :: l
            INTEGER(4) :: i

            CHARACTER(:), ALLOCATABLE :: str


            PRINT *, 'WalkList(', list%compType, ',', rc, ')'

            m = NumElements( list )
            DO i = 0, m
                PRINT *, 'WalkList() ', i, m, rc

                SELECT CASE ( i )
                    CASE( 4 )
                        str = ACHAR(61 + m)
                        l = LEN( str )

                        ASSOCIATE(b => list%compBase%compBase%compBase%compBase)
                            CALL VerifyBase(b, l, str, (i /= m), (rc + 25_4))
                            CALL WalkList(b, (rc + (m * 5_4)))
                        END ASSOCIATE

                    CASE( 3 )
                        str = ACHAR(62 + m)
                        l = LEN( str )

                        ASSOCIATE(b => list%compBase%compBase%compBase)
                            CALL VerifyBase(b, l, str, (i /= m), (rc + 25_4))
                            CALL WalkList(b, (rc + (m * 5_4)))
                        END ASSOCIATE

                    CASE( 2 )
                        str = ACHAR(63 + m)
                        l = LEN( str )

                        ASSOCIATE(b => list%compBase%compBase)
                            CALL VerifyBase(b, l, str, (i /= m), (rc + 25_4))
                            CALL WalkList(b, (rc + (m * 5_4)))
                        END ASSOCIATE

                    CASE( 1 )
                        IF (m == (N + 1)) THEN
                            str = char2Str

                        ELSE
                            str = ACHAR(64 + m)
                        END IF

                        l = LEN( str )

                        ASSOCIATE(b => list%compBase)
                            CALL VerifyBase(b, l, str, (i /= m), (rc + 25_4))
                            CALL WalkList(b, (rc + (m * 5_4)))
                        END ASSOCIATE

                    CASE( 0 )
                        IF (m == (N + 1)) THEN
                            str = char3Str

                        ELSE IF (m == N) THEN
                            str = char2Str

                        ELSE
                            str = ACHAR(65 + m)
                        END IF

                        l = LEN( str )

                        ASSOCIATE(b => list)
                            CALL VerifyBase(b, l, str, (i /= m), (rc + 25_4))
                        END ASSOCIATE

                    CASE DEFAULT
                        CALL zzrc( rc )
                END SELECT
            END DO

        END SUBROUTINE WalkList

        RECURSIVE INTEGER FUNCTION NumElements( list )
            TYPE(tBase(*)) :: list


            NumElements = 0
            IF ( ASSOCIATED( list%compBase ) ) THEN
                NumElements = NumElements( list%compBase ) + 1
            END IF

        END FUNCTION NumElements

END PROGRAM componentSelector05
