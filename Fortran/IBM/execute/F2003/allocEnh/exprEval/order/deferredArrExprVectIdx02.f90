!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : deferredArrExprVectIdx02 - Order of Expression
!*                               Evaluation
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : October  6, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is
!*                               an Allocated ALLOCATABLE Array of Type
!*                               CHARACTER with Deferred Length (Indexed
!*                               using a Vector Subscript)
!*  SECONDARY FUNCTIONS TESTED : expr references variable (also Indexed
!*                               using a Vector Subscript), and will have
!*                               a different Length Type Parameter Result
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

PROGRAM deferredArrExprVectIdx02

    CHARACTER(2) :: charArr( -5:5 )
    CHARACTER(:), ALLOCATABLE :: charArrAlloc( : )


    DO i = -5, 5
        WRITE(charArr( i ), '(I2)') i
    END DO

    PRINT *, 'Source:          ', charArr


    charArrAlloc = charArr
    PRINT *, 'Assignment:      ', charArrAlloc


    charArrAlloc( Idx( charArrAlloc( 4:-4:-1 ) ) ) =&
                charArrAlloc( Idx( charArrAlloc( -4:4 ) ) )
    PRINT *, 'Array Section:   ', charArrAlloc


    charArrAlloc( Idx( charArrAlloc( 0:-4:-1 ) ) ) =&
                charArrAlloc( Idx( charArrAlloc( 0:4 ) ) ) //&
                        charArrAlloc( Idx( charArrAlloc( 0:-4:-1 ) ) )
    PRINT *, 'Concatenation:   ', charArrAlloc


    charArrAlloc( Idx( charArrAlloc( (/ -5,5 /) ) ) )( :1 ) =&
                charArrAlloc( Idx( charArrAlloc( (/ 5,5 /) ) ) )( :1 )
    PRINT *, 'SubString:       ', charArrAlloc


    CONTAINS

        FUNCTION Idx( chrArr )
            CHARACTER(*) :: chrArr( : )

            INTEGER :: Idx( SIZE( chrArr ) )

            DO i = 1, SIZE( chrArr )
                READ(chrArr( i )( :2 ), '(I2)') Idx( i )
            END DO

        END FUNCTION Idx

END PROGRAM deferredArrExprVectIdx02
