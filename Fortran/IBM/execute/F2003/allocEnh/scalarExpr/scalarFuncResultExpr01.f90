!*  ===================================================================
!*
!*                               Scalar and variable is an Array
!*
!*  DATE                       : October 30, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array,
!*  SECONDARY FUNCTIONS TESTED : and expr is a Scalar Result from a FUNCTION.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                :
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
!*  If expr is a scalar and variable is an array, the expr is treated as if
!*  it were an array of the same shape as variable with every element of the
!*  array equal to the scalar value of expr.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM scalarFuncResultExpr01

    INTERFACE
        REAL FUNCTION RealFunc( c )
             COMPLEX :: c
        END FUNCTION RealFunc
    END INTERFACE


    COMPLEX, ALLOCATABLE :: complexArrAlloc( :,:,: )


    complexArrAlloc =&
        RESHAPE([ (((CMPLX((REAL( k ) + (REAL( j ) / 10.)), i),&
                    k = 1, 3), j = 1, 3), i = 1, 3) ], (/ 3,3,3 /))
    CALL CheckIt( 10_4 )


    complexArrAlloc = RealFunc( complexArrAlloc( 2,2,2 ) )
    CALL CheckIt( 20_4 )

    complexArrAlloc = ComplexFunc(9, 8)
    CALL CheckIt( 30_4 )

    complexArrAlloc = IACHAR( CHAR( INT( complexArrAlloc( 1,1,1 ) ) ) )
    CALL CheckIt( 40_4 )


    CONTAINS


        SUBROUTINE CheckIt( failRc )
            INTEGER(4) :: failRC


            IF (.NOT. ALLOCATED( complexArrAlloc )) CALL zzrc( failRC )


            PRINT *
            PRINT *, failRC, SIZE(complexArrAlloc, 1),&
                     SIZE(complexArrAlloc, 2), SIZE(complexArrAlloc, 3)

            DO i = 1, SIZE(complexArrAlloc, 1)
                PRINT '(3(3("(",F3.1,",",F3.1,") ")," "))',&
                                        complexArrAlloc( i,:,: )
            END DO


            IF (SIZE(complexArrAlloc, 1) /= 3) CALL zzrc(( failRC + 1_4 ) )
            IF (SIZE(complexArrAlloc, 2) /= 3) CALL zzrc(( failRC + 2_4 ) )
            IF (SIZE(complexArrAlloc, 3) /= 3) CALL zzrc(( failRC + 3_4 ) )

        END SUBROUTINE CheckIt


        COMPLEX FUNCTION ComplexFunc(i, j)
            INTEGER :: i
            INTEGER :: j


            ComplexFunc = i + (REAL( j ) / 10.)

        END FUNCTION ComplexFunc

END PROGRAM scalarFuncResultExpr01


REAL FUNCTION RealFunc( c )
     COMPLEX :: c


    RealFunc = c

END FUNCTION RealFunc
