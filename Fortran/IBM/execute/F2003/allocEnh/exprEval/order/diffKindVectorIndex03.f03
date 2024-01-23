!*  ===================================================================
!*
!*                               Evaluation
!*
!*  DATE                       : October 11, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Intrinsic
!*                               Type COMPLEX
!*  SECONDARY FUNCTIONS TESTED : expr references elements of variable (Indexed
!*                               using a Vector Subscript), and will have a
!*                               different Length Type Parameter Result
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  diffKindVectorIndex03:   Default
!*  diffKindVectorIndex03a:  -qrealsize=4
!*  diffKindVectorIndex03b:  -qrealsize=8
!*
!*  7.4.1.1 General form
!*
!*  R734 assignment-stmt  is  variable = expr
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

PROGRAM diffKindVectorIndex03

    COMPLEX :: defaultKind
    INTEGER :: defaultComplexKind = KIND( defaultKind )

    COMPLEX, ALLOCATABLE :: complexArrAlloc( :,: )

    COMPLEX :: complexArr( 5,5 ) =&
        RESHAPE((/ ((CMPLX(j, i), j = 1, 5), i = 1, 5) /), (/ 5,5 /))


    complexArrAlloc = complexArr
    IF (KIND( complexArrAlloc ) /= defaultComplexKind) ERROR STOP 10_4
    CALL Dump( 'Assignment(Default Kind) (5x5):' )


    complexArrAlloc =&
        CMPLX(REAL( complexArrAlloc( (/ 5,4,3,2,1 /),(/ 1,2,3,4,5 /) ) ),&
              AIMAG( complexArrAlloc( (/ 1,2,3,4,5 /),(/ 5,4,3,2,1 /) ) ), 4)
    IF (KIND( complexArrAlloc ) /= defaultComplexKind) ERROR STOP 20_4
    CALL Dump( 'COMPLEX(4) (5x5):' )


    complexArrAlloc =&
        CMPLX(REAL( complexArrAlloc( (/ 1,2,3,4,5 /),(/ 1,2,3,4,5 /) ) ),&
              AIMAG( complexArrAlloc( (/ 5,4,3,2,1 /),(/ 5,4,3,2,1 /) ) ), 8)
    IF (KIND( complexArrAlloc ) /= defaultComplexKind) ERROR STOP 30_4
    CALL Dump( 'COMPLEX(8) (5x5):' )


    complexArrAlloc =&
        CMPLX(REAL( complexArrAlloc( (/ 5,4,3,2,1 /),(/ 5,4,3,2,1 /) ) ),&
              AIMAG( complexArrAlloc( (/ 1,2,3,4,5 /),(/ 1,2,3,4,5 /) ) ), 16)
    IF (KIND( complexArrAlloc ) /= defaultComplexKind) ERROR STOP 40_4
    CALL Dump( 'COMPLEX(16) (5x5):' )


    CONTAINS


        SUBROUTINE Dump( title )
            CHARACTER(*) :: title

            PRINT *
            PRINT *, title

            DO i = 1, 5
                PRINT 10, complexArrAlloc( :,i )
10              FORMAT(' ',5('(',F3.1,',',F3.1,') '))
            END DO

        END SUBROUTINE Dump

END PROGRAM diffKindVectorIndex03
