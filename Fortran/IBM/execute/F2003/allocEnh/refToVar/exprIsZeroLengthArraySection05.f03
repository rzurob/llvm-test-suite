!*  ===================================================================
!*
!*                               and/or expr Contain References to variable
!*
!*  DATE                       : October 18, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Type CHARACTER
!*  SECONDARY FUNCTIONS TESTED : and expr is a Zero Length Array Section of
!*                               variable
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

PROGRAM exprIsZeroLengthArraySection05

    CHARACTER(5) :: charArr( 11 ) =&
        (/ 'ABCDE', 'fghij', 'KLMNO', 'pqrst', 'UVWXY',&
           'z123Z', 'abcde', 'FGHIJ', 'klmno', 'PQRST', 'uvwxy' /)

    CHARACTER(5), ALLOCATABLE :: charArrAlloc( :,: )


    ALLOCATE(charArrAlloc( -5:5,-4:4 ), SOURCE=RESHAPE(&
        (/ ((charArr( j ), j = 1, 11), i = 1, 9) /), (/ 11,9 /)))
    CALL CheckIt(RESHAPE((/ 11,-5,5,5, 9,-4,4,5 /), (/ 4,2 /)),&
                                                    'ALLOCATE():', 10_4)


    charArrAlloc = charArrAlloc( 4:-4,-3:3 )
    CALL CheckIt(RESHAPE((/ 0,1,0,5, 7,1,7,5 /), (/ 4,2 /)),&
                                        'Intrinsic Assignment:', 20_4)


    CONTAINS


        SUBROUTINE CheckIt(limitsList, title, failRC)
            INTEGER :: limitsList( 4,2 )
            CHARACTER(*) :: title
            INTEGER(4) :: failRC

            INTEGER :: cAALimits( 4,2 )


            IF (.NOT. ALLOCATED( charArrAlloc )) CALL zzrc( failRC )

            PRINT *
            PRINT *, title

            DO i = 1, 2
                cAALimits( 1,i ) = SIZE(charArrAlloc, i)
                cAALimits( 2,i ) = LBOUND(charArrAlloc, i)
                cAALimits( 3,i ) = UBOUND(charArrAlloc, i)
                cAALimits( 4,i ) = LEN( charArrAlloc )


                PRINT *, i, ')  Found:     ', cAALimits( :,i )
                PRINT *,  '     Expected:  ', limitsList( :,i )
            END DO


            IF ( ANY( (limitsList /= cAALimits) ) ) CALL zzrc( failRC + 1_4 )

        END SUBROUTINE CheckIt

END PROGRAM exprIsZeroLengthArraySection05
