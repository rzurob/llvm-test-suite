!*  ===================================================================
!*
!*                               Conversion
!*
!*  DATE                       : November  6, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of CHARACTER
!*                               (with Deferred Length),
!*  SECONDARY FUNCTIONS TESTED : and expr is the Intrinsic Function RESHAPE()
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
!*  For a numeric intrinsic assignment statement, variable and expr may have
!*  different numeric types or different kind type parameters, in which case
!*  the value of expr is converted to the type and kind type parameter of
!*  variable according to the rules of Table 7.9.
!*
!*  Table 7.9: Numeric conversion and the assignment statement
!*  Type of variable Value Assigned
!*  integer     INT (expr, KIND = KIND (variable))
!*  real        REAL (expr, KIND = KIND (variable))
!*  complex     CMPLX (expr, KIND = KIND (variable))
!*  Note: The functions INT, REAL, CMPLX, and KIND are the generic functions
!*  defined in 13.7.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM exprIsIntrinsicFunc05

    CHARACTER(1) :: char
    CHARACTER(:), ALLOCATABLE :: pattern
    CHARACTER(:), ALLOCATABLE :: charArrAlloc( :,: )


    pattern = ''
    ALLOCATE(CHARACTER(0) :: charArrAlloc( 27,1 ))


    DO i = 0, 3
        j = 3 ** i
        k = 3 ** (3 - i)
        m = i + 1

        WRITE(char, '(I1)') m
        pattern = pattern // char

        CALL CatReshape(j, k, charArrAlloc, char)
        CALL Dump(j, k, m, pattern, INT((m * 10), 4))
    END DO


    CONTAINS


        SUBROUTINE CatReshape(s1, s2, arr, chr)
            INTEGER :: s1
            INTEGER :: s2
            CHARACTER(:), ALLOCATABLE :: arr( :,: )
            CHARACTER(1) :: chr


            arr = RESHAPE((arr // chr), [ s1,s2 ])

        END SUBROUTINE CatReshape


        SUBROUTINE Dump(s1, s2, l, p, rc)
            INTEGER :: s1
            INTEGER :: s2
            INTEGER :: l
            CHARACTER(*) :: p
            INTEGER(4) :: rc

            INTEGER :: i


            IF (.NOT. ALLOCATED( charArrAlloc )) CALL zzrc( rc )


            PRINT *
            PRINT *, rc, SIZE(charArrAlloc, 1), "(", s1, ")",&
                         SIZE(charArrAlloc, 2), "(", s2, ")",&
                         LEN( charArrAlloc ), "(", l, ") (", p, ")"


            IF (LEN( charArrAlloc ) /= l) CALL zzrc( (rc + 1_4) )

            IF (SIZE(charArrAlloc, 1) /= s1) CALL zzrc( (rc + 2_4) )
            IF (SIZE(charArrAlloc, 2) /= s2) CALL zzrc( (rc + 3_4) )


            DO i = 1, SIZE(charArrAlloc, 1)
                PRINT *, i, charArrAlloc( i,: )
            END DO


            IF ( ANY( (charArrAlloc /= p) ) ) CALL zzrc( (rc + 4_4) )

        END SUBROUTINE Dump

END PROGRAM exprIsIntrinsicFunc05
