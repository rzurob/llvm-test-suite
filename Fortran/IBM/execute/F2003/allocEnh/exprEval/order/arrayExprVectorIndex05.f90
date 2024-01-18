!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : arrayExprVectorIndex05 - Order of Expression
!*                               Evaluation
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : September 27, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of CHARACTER
!*                               Intrinsic Type (Indexed using a Vector
!*                               Subscript)
!*  SECONDARY FUNCTIONS TESTED : expr references variable, and will have a
!*                               different Shape Result
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

PROGRAM arrayExprVectorIndex05

    CHARACTER(7), ALLOCATABLE :: charArrAlloc( : )


    ALLOCATE(charArrAlloc( 7 ),&
        SOURCE=(/ '1,abcde', '2,FGHIJ', '3,klmno',&
                  '4,PQRST', '5,uvwxy', '6,ZABCD', '7,Efghi' /))


    PRINT 10, '', charArrAlloc
10  FORMAT(A6,' (',6('"',A7,'",'),'"',A7,'")')

    PRINT 20, '', charArrAlloc( 1:4 )
    PRINT 20, 'Before', charArrAlloc( (/ 1,3,5,7 /) )
20  FORMAT(A6,' (',3('"',A7,'",'),'"',A7,'")')


    charArrAlloc( Idx( charArrAlloc( (/ 1,3,5,7 /) ) ) ) =&
        charArrAlloc( :4 )( :2 ) // charArrAlloc( :4 )( 6: ) //&
        charArrAlloc( :4 )( 5:5 ) // charArrAlloc( :4 )( 3:4 )


    PRINT 20, 'After', charArrAlloc( (/ 1,3,5,7 /) )
    PRINT 20, '', charArrAlloc( 1:4 )
    PRINT 10, '', charArrAlloc


    CONTAINS

        FUNCTION Idx( charArr )
            CHARACTER(7) :: charArr( : )
            INTEGER :: Idx( SIZE( charArr ) )

            DO i = 1, SIZE( charArr )
                READ(charArr( i ), '(I1)') Idx( i )
            END DO

        END FUNCTION Idx

END PROGRAM arrayExprVectorIndex05
