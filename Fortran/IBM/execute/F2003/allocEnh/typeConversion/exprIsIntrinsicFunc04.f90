!*  ===================================================================
!*
!*                               Conversion
!*
!*  DATE                       : November  6, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is
!*                               an Unallocated/Allocated ALLOCATABLE Array
!*                               of CHARACTER (with Deferred Length),
!*  SECONDARY FUNCTIONS TESTED : and expr is the Intrinsic Function SPREAD()
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

PROGRAM exprIsIntrinsicFunc04

    CHARACTER(:), ALLOCATABLE :: pattern
    CHARACTER(:), ALLOCATABLE :: charArrAlloc( :,: )


    pattern = 'AbC'
    charArrAlloc = RESHAPE(SPREAD(pattern, 1, 1), [ 1,1 ])
    CALL Dump(pattern, 1, 1, 3, 10_4)


    pattern = Trans1( charArrAlloc( 1,1 ) )
    charArrAlloc = SPREAD(RESHAPE(Trans1( charArrAlloc ), [ 1 ]), 1, 5)
    CALL Dump(pattern, 5, 1, 7, 20_4)

    pattern = Trans2( charArrAlloc( 1,1 ) )
    charArrAlloc = SPREAD(RESHAPE(Trans2( charArrAlloc ), [ 5 ]), 1, 5)
    CALL Dump(pattern, 5, 5, 11, 30_4)


    CONTAINS


        ELEMENTAL FUNCTION Trans1( char )
            CHARACTER(3), INTENT(in) :: char

            CHARACTER(7) :: Trans1


            Trans1 = char // 'd' // char

        END FUNCTION Trans1


        ELEMENTAL FUNCTION Trans2( char )
            CHARACTER(7), INTENT(in) :: char

            CHARACTER(11) :: Trans2


            Trans2 = char( :4 ) // 'E' // char( 3: ) // 'f'

        END FUNCTION Trans2


        SUBROUTINE Dump(p, s1, s2, l, rc)
            CHARACTER(*) :: p
            INTEGER :: s1
            INTEGER :: s2
            INTEGER :: l
            INTEGER(4) :: rc


            IF (.NOT. ALLOCATED( charArrAlloc )) CALL zzrc( rc )


            PRINT *
            PRINT *, SIZE(charArrAlloc, 1), "(", s1, ")",&
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

END PROGRAM exprIsIntrinsicFunc04
