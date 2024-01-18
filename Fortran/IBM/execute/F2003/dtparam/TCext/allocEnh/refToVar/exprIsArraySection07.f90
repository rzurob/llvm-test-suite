! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/refToVar/exprIsArraySection07.f
! opt variations: -qnol -qnodeferredlp

!*  ===================================================================
!*
!*                               expr Contain References to variable
!*
!*  DATE                       : October 17, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Derived Type
!*  SECONDARY FUNCTIONS TESTED : expr is an Array Section of variable
!*
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

    TYPE :: tBase(N1,K1)    ! (20,16)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
        COMPLEX(K1)   :: b
    END TYPE tBase

END MODULE mBase


MODULE mDerived
    USE mBase

    TYPE, EXTENDS(tBase) :: tDerived    ! (20,16)
        DOUBLE PRECISION :: d
    END TYPE tDerived

END MODULE mDerived

PROGRAM exprIsArraySection07
    USE mDerived

    TYPE(tDerived(20,16)) :: derivedArr( 10,10,10 )
    TYPE(tDerived(:,16)), ALLOCATABLE :: derivedArrAlloc( :,:,: )


    derivedArr= RESHAPE((/ (((tDerived(20,16)(CMPLX(k,j,16),REAL(i,16)),&
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
            TYPE(tDerived(*,16)) :: dA( :,:,: )
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
