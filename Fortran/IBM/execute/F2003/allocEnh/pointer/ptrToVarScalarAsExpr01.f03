!*  ===================================================================
!*
!*  DATE                       : October 25, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is a an
!*                               Allocated ALLOCATABLE Scalar of CHARACTER
!*  SECONDARY FUNCTIONS TESTED : and expr uses a POINTER that has a TARGET of
!*                               variable and the value of expr has a different
!*                               Length Type Parameter Result
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 3
!*
!*  DESCRIPTION                :
!*
!*  7.4.1.1 General form
!*
!*  R734 assignment-stmt  is  variable = expr
!*
!*  7.4.1.3 Interpretation of intrinsic assignments
!*
!*  If variable is a pointer, the value of expr is assigned to the target
!*  of variable.
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

PROGRAM ptrToVarScalarAsExpr01

    INTEGER(4) :: rc = 10_4
    INTEGER, PARAMETER :: N = 12

    CHARACTER(N), PARAMETER :: testStr = '123456789abc'
    CHARACTER(N), POINTER :: charScalarPtr
    CHARACTER(N), TARGET, ALLOCATABLE :: charScalarAlloc


    charScalarAlloc = testStr
    CALL Check(testStr, rc)


    charScalarPtr => charScalarAlloc
    charScalarAlloc =  charScalarPtr

    rc = rc + 5_4
    CALL Check(testStr, rc)


    PRINT *

    DO i = 1, N
        charScalarAlloc =  charScalarPtr( N: ) // charScalarPtr

        rc = rc + 5_4
        CALL Check((testStr( (N - i + 1): ) // testStr( :(N - i) )), rc)
    END DO


    PRINT *

    DO i = 1, N
        charScalarAlloc =  charScalarPtr( 2:(N - i + 1) )

        rc = rc + 5_4
        CALL Check(testStr( (i + 1): ), rc)
    END DO


    CONTAINS


        SUBROUTINE Check(pattern, failRC)
            CHARACTER(*) :: pattern
            INTEGER(4) :: failRC


            IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( failRC )

            PRINT *, failRC, LEN( charScalarAlloc ), '(', N, '), "',&
                                charScalarAlloc, '", (', pattern, ')'

            IF (LEN( charScalarAlloc ) /= N)    CALL zzrc( (failRC + 1_4) )
            IF (charScalarAlloc /= pattern)     CALL zzrc( (failRC + 2_4) )

        END SUBROUTINE Check

END PROGRAM ptrToVarScalarAsExpr01
