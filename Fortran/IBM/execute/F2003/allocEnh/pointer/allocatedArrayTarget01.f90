!*  ===================================================================
!*
!*  DATE                       : October 13, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is a
!*                               POINTER to an Allocated ALLOCATABLE Array of
!*                               Intrinsic Type (INTEGER, REAL, COMPLEX), and
!*  SECONDARY FUNCTIONS TESTED : the value of expr has a different Shape
!*                               Result
!*
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

PROGRAM allocatedArrayTarget01

    INTEGER, POINTER :: intArrPtr( :,: )
    INTEGER, TARGET, ALLOCATABLE :: intArrAlloc( :,: )

    REAL, POINTER :: realArrPtr( : )
    REAL, TARGET, ALLOCATABLE :: realArrAlloc( : )

    COMPLEX, POINTER :: complexArrPtr( :,:,: )
    COMPLEX, TARGET, ALLOCATABLE :: complexArrAlloc( :,:,: )


    ALLOCATE(intArrAlloc( 10,10 ), SOURCE=RESHAPE(&
            (/ ((((i * 10) + j), j = 0, 9), i = 0, 9) /), (/ 10,10 /)))

    intArrPtr => intArrAlloc

    CALL CheckInt(10, 10, 10_4)


    intArrPtr(:,:5) = RESHAPE((/&
        (((50 - ((i * 10) + j)), j = 0, 9), i = 0, 4) /), (/ 10,5 /))
    CALL CheckInt(10, 10, 20_4)


    PRINT *

    ALLOCATE(realArrAlloc( 10 ), SOURCE=(/ (REAL( i ), i = 10, 1, -1) /))
    realArrPtr => realArrAlloc
    CALL CheckReal(10, 30_4)


    realArrPtr = (/ (REAL( i ), i = 99, 90, -1) /)
    CALL CheckReal(10, 40_4)


    ALLOCATE(complexArrAlloc( 3,3,3 ), SOURCE=RESHAPE(&
        (/ (((CMPLX(k,j), k = 1, 3), j = 4, 6), i = 7, 9) /), (/ 3,3,3 /)))

    complexArrPtr => complexArrAlloc
    CALL CheckComplex(3, 3, 3, 50_4)


    complexArrPtr =&
        RESHAPE((/ ((CMPLX(j, i), j = 9, 1, -1), i = 3, 1, -1) /), (/ 3,3,3 /))
    CALL CheckComplex(3, 3, 3, 60_4)

    CONTAINS

        SUBROUTINE CheckInt(iMax, jMax, failRC)
            INTEGER :: iMax
            INTEGER :: jMax
            INTEGER(4) :: failRC

            IF (.NOT. ALLOCATED( intArrAlloc )) CALL zzrc( failRC )

            PRINT *
            DO i = 1, SIZE(intArrAlloc, 1)
                PRINT '(10(I3))', intArrAlloc( :,i )
            END DO

            IF (SIZE(intArrAlloc, 1) /= iMax)   CALL zzrc( (failRC + 1_4) )
            IF (SIZE(intArrAlloc, 2) /= jMax)   CALL zzrc( (failRC + 2_4) )

        END SUBROUTINE CheckInt

        SUBROUTINE CheckReal(iMax, failRC)
            INTEGER :: iMax
            INTEGER(4) :: failRC

            IF (.NOT. ALLOCATED( realArrAlloc )) CALL zzrc( failRC )

            PRINT '(10(F5.1))', realArrAlloc

            IF (SIZE(realArrAlloc, 1) /= iMax)   CALL zzrc( (failRC + 1_4) )

        END SUBROUTINE CheckReal

        SUBROUTINE CheckComplex(iMax, jMax, kMax, failRC)
            INTEGER :: iMax
            INTEGER :: jMax
            INTEGER(4) :: failRC

            IF (.NOT. ALLOCATED( complexArrAlloc )) CALL zzrc( failRC )

            PRINT *
            DO i = 1, SIZE(complexArrAlloc, 1)
                PRINT *, 'Row:', i
                DO j = 1, SIZE(complexArrAlloc, 2)
                    PRINT 10, complexArrAlloc( :,j,i )
10                  FORMAT(3('(',F4.1,',',F4.1,') '))
                END DO
            END DO

            IF (SIZE(complexArrAlloc, 1) /= iMax)   CALL zzrc( (failRC + 1_4) )
            IF (SIZE(complexArrAlloc, 2) /= jMax)   CALL zzrc( (failRC + 2_4) )
            IF (SIZE(complexArrAlloc, 3) /= kMax)   CALL zzrc( (failRC + 3_4) )

        END SUBROUTINE CheckComplex

END PROGRAM allocatedArrayTarget01
