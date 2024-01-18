!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : ptrToVarArrAsExpr01 - variable is a POINTER
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : October 20, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is a an
!*                               Allocated ALLOCATABLE Array of Intrinsic Type
!*                               (INTEGER)
!*  SECONDARY FUNCTIONS TESTED : and expr uses a POINTER that has a TARGET of
!*                               variable and the value of expr has a different
!*                               Shape Result
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

PROGRAM ptrToVarArrAsExpr01

    INTEGER(4) :: i

    INTEGER, POINTER :: ptrToIntArr( : )
    INTEGER, TARGET, ALLOCATABLE :: intArrAlloc( : )


    ALLOCATE(intArrAlloc( 10 ), SOURCE=(/ (i, i = 1, 10) /))
    CALL Dump(10_4, 10_4)


    DO i = 1_4, 4_4
        ptrToIntArr => intArrAlloc
        intArrAlloc = ptrToIntArr( 2:(SIZE( intArrAlloc ) - 1) )

        CALL Dump((10_4 - (i * 2_4)), (10_4 + (i * 10_4)))
    END DO


    PRINT *


    CALL Dump(2_4, 60_4)

    DO i = 1_4, 5_4
        ptrToIntArr => intArrAlloc
        intArrAlloc = (/ ptrToIntArr( 1 ), ptrToIntArr,&
                         ptrToIntArr( SIZE( intArrAlloc ) ) /)

        CALL Dump((2_4 + (2_4 * i)), (60_4 + (i * 10_4)))
    END DO


    CONTAINS


        SUBROUTINE Dump(size1, failRC)
            INTEGER(4) :: size1
            INTEGER(4) :: failRC


            IF (.NOT. ALLOCATED( intArrAlloc ))     CALL zzrc( failRC )


            PRINT *, KIND( intArrAlloc ), "(", KIND( j ),")	", &
                     SIZE( intArrAlloc ), "(", size1, ")	", intArrAlloc

            IF (SIZE( intArrAlloc ) /= size1)       CALL zzrc( (failRC + 1_4) )
            IF (KIND( intArrAlloc ) /= KIND( j ))   CALL zzrc( (failRC + 2_4) )

        END SUBROUTINE Dump

END PROGRAM ptrToVarArrAsExpr01
