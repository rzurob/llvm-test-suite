!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : deferredArrExprVectIdx01 - Order of Expression
!*                               Evaluation
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : October  6, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is
!*                               an Allocated ALLOCATABLE Array of Type
!*                               CHARACTER with Deferred Length, and expr
!*                               references variable, and will have a
!*                               different Length Type Parameter Result
!*  SECONDARY FUNCTIONS TESTED : either variale or expr are Indexed using
!*                               a Vector Subscript)
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  7.4.1.1 General form
!*
!*  R734 assignment-stmt  is  variable = expr
!*
!*
!*  7.4.1.3 Interpretation of intrinsic assignments
!*
!*  If variable is an allocated allocatable variable, it is deallocated if
!*  expr is an array of different shape or any of the corresponding length
!*  type parameter values of variable and expr differ. If variable is or
!*  becomes an unallocated allocatable variable, then it is allocated with
!*  each deferred type parameter equal to the corresponding type parameters
!*  of expr, with the shape of expr, and with each lower bound equal to the
!*  corresponding element of LBOUND(expr).
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM deferredArrExprVectIdx01

    CHARACTER(:), ALLOCATABLE :: charArrAlloc( :,: )

    CHARACTER(3) :: char3Arr( 0:9,0:9 )


    DO i = 0, 9
        DO j = 0, 9
            WRITE(char3Arr( j,i ), '(I1,",",I1)') (9 - j), i
        END DO
    END DO

    PRINT *, 'Starting Data:'

    DO i = 0, 9
        PRINT 10, (char3Arr( j,i ), j = 0, 9)
    END DO

10  FORMAT(10("(",A3,")"))


    charArrAlloc = char3Arr

    PRINT *
    PRINT *, 'Initial Assignment:'

    DO i = 0, 9
        PRINT 10, charArrAlloc( :,i )
    END DO


    charArrAlloc( :,Idx( charArrAlloc( :,0 ) ) ) = charArrAlloc // charArrAlloc

    CALL DumpIt('variable with Vector Subscript:', 0, 9)


    charArrAlloc = charArrAlloc( Idx( charArrAlloc( :,0 ) ),: ) //&
                        charArrAlloc( Idx( charArrAlloc( :,0 ) ),: )

    CALL DumpIt('expr with Vector Subscript (expr longer than variable):',1,10)


    !
    !  LBOUND( charArrAlloc ) == (/ 1,1 /)
    !  Select an 8x8
    !
    charArrAlloc =&
        charArrAlloc(&
            Idx( charArrAlloc( 3:10,1 ) ),Idx( charArrAlloc( 3:10,1 ) )&
                    )( 1:1 ) // ':' //&
        charArrAlloc(&
            Idx( charArrAlloc( 3:10,1 ) ),Idx( charArrAlloc( 3:10,1 ) )&
                    )( 6:6 )

    CALL DumpIt('expr with Vector Subscript (expr shorter than variable):',1,8)


    CONTAINS


        FUNCTION Idx( chrArr )
            CHARACTER(*) :: chrArr( : )

            INTEGER :: Idx( SIZE( chrArr ) )

            DO i = 1, SIZE( chrArr )
                READ(chrArr( i )( 1:1 ), '(I1)') Idx( i )
            END DO

        END FUNCTION Idx


        SUBROUTINE DumpIt(title, lb, ub)
            CHARACTER(*), INTENT(in) :: title
            INTEGER, INTENT(in) :: lb
            INTEGER, INTENT(in) :: ub

            PRINT *
            PRINT *, title

            DO i = lb, ub
                PRINT *, (charArrAlloc( j,i ), j = lb, ub)
            END DO

        END SUBROUTINE DumpIt

END PROGRAM deferredArrExprVectIdx01
