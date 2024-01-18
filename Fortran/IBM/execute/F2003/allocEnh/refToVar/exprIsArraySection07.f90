!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : exprIsArraySection07 - variable and/or
!*                               expr Contain References to variable
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : October 17, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Derived Type
!*  SECONDARY FUNCTIONS TESTED : expr is an Array Section of variable
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 4
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
!*  Both variable and expr may contain references to any portion of variable.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mBase

    TYPE :: tBase
        COMPLEX(16) :: b
    END TYPE tBase

END MODULE mBase


MODULE mDerived
    USE mBase

    TYPE, EXTENDS(tBase) :: tDerived
        DOUBLE PRECISION :: d
    END TYPE tDerived

END MODULE mDerived

PROGRAM exprIsArraySection07
    USE mDerived

    TYPE(tDerived) :: derivedArr( 10,10,10 )
    TYPE(tDerived), ALLOCATABLE :: derivedArrAlloc( :,:,: )


    derivedArr= RESHAPE((/ (((tDerived(CMPLX(k,j,16),REAL(i,16)),&
                    k = 1, 10), j = 1, 10), i = 1, 10) /), (/ 10,10,10 /))


    derivedArrAlloc = derivedArr
    CALL CheckIt((/ 10,10,10 /), derivedArr, 10_4)


    derivedArrAlloc =&
        derivedArrAlloc( :,:,(/ ((MOD(i, 10) + 1), i = 0, 14) /) )
    CALL CheckIt((/ 10,10,15 /),&
                derivedArr( :,:,(/ 1,2,3,4,5,6,7,8,9,10,1,2,3,4,5 /) ), 20_4)

    derivedArrAlloc =&
        derivedArrAlloc( (/ ((MOD(i, 10) + 1), i = 0, 14) /),:,: )
    CALL CheckIt((/ 15,10,15 /), derivedArr(&
        (/ 1,2,3,4,5,6,7,8,9,10,1,2,3,4,5 /),:,&
        (/ 1,2,3,4,5,6,7,8,9,10,1,2,3,4,5 /) ), 30_4)

    derivedArrAlloc = derivedArrAlloc( :5,6:10,11:15 )
    CALL CheckIt((/ 5,5,5 /), derivedArr( :5,6:,:5 ), 40_4)

    derivedArrAlloc = derivedArrAlloc( (/ ((MOD(i, 5) + 1), i = 0, 9) /),&
                                       (/ ((MOD(i, 5) + 1), i = 0, 9) /),&
                                       (/ ((MOD(i, 5) + 1), i = 0, 9) /) )
    CALL CheckIt((/ 10,10,10 /),derivedArr((/  1,2,3,4,5,1,2,3,4,5  /),&
                                           (/ 6,7,8,9,10,6,7,8,9,10 /),&
                                           (/  1,2,3,4,5,1,2,3,4,5  /)), 50_4)


    CONTAINS


        SUBROUTINE CheckIt(sizeList, dA, failRC)
            INTEGER :: sizeList( 3 )
            TYPE(tDerived) :: dA( :,:,: )
            INTEGER(4) :: failRC

            INTEGER(4) :: i


            IF (.NOT. ALLOCATED( derivedArrAlloc )) CALL zzrc( failRC )

            PRINT *
            PRINT *, (failRC / 10_4)
            DO i = 1_4, 3_4
                PRINT *, SIZE(derivedArrAlloc, i), "(", sizeList( i ), ")"
                IF (SIZE(derivedArrAlloc, i) /= sizeList( i ))&
                                            CALL zzrc( (failRC + i) )
            END DO

            IF ( ANY( (derivedArrAlloc%b /= dA%b) ) )&
                                CALL zzrc( (failRC + 4_4) )
            IF ( ANY( (derivedArrAlloc%d /= dA%d) ) )&
                                CALL zzrc( (failRC + 5_4) )

        END SUBROUTINE CheckIt

END PROGRAM exprIsArraySection07
