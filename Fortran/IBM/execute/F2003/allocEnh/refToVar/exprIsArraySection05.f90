!*  ===================================================================
!*
!*                               expr Contain References to variable
!*
!*  DATE                       : October 19, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of CHARACTER
!*  SECONDARY FUNCTIONS TESTED : expr is an Array Section of variable
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
!*  Both variable and expr may contain references to any portion of variable.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM exprIsArraySection05

    CHARACTER(7) :: charArr( 10 ) =&
        (/ 'abcdefg', 'HIJKLMN', 'opqrstu', 'VWXYZAB', 'cdefghi',&
           'JKLMNOP', 'qrstuvw', 'XYZabcd', 'EFGhijk', 'lmnOPQr' /)

    CHARACTER(7), ALLOCATABLE :: charArrAlloc( : )


    charArrAlloc = charArr
    CALL CheckIt((/ 10,1,10,7 /), 'Initialization (10):', 10_4)

    charArrAlloc = charArrAlloc( 2:9 )
    CALL CheckIt((/ 8,1,8,7 /), 'Array Section 2:9 (8):', 20_4)

    charArrAlloc = charArrAlloc( 2:7 )( 2:6 )
    CALL CheckIt((/ 6,1,6,7 /),&
                'Array Section 2:7 (Sub-String 2:6) (6):', 30_4)

    charArrAlloc = charArrAlloc( 2:5 )( :3 ) //&
                   charArrAlloc( 2:5 )( 5: ) //&
                   charArrAlloc( 2:5 )( 3:5 )
    CALL CheckIt((/ 4,1,4,7 /),&
            'Array Section 2:5 (Concatenate 1:3 + 5:7 + 3:5) 2:7 (4):', 40_4)


    CONTAINS


        SUBROUTINE CheckIt(limits, title, failRC)
            INTEGER :: limits( 4 )
            CHARACTER(*) :: title
            INTEGER(4) :: failRC

            CHARACTER(13) :: fmt


            PRINT *
            PRINT *, title


            IF (.NOT. ALLOCATED( charArrAlloc ))    CALL zzrc( failRC )


            PRINT 10, 'Expected:', limits
            PRINT 10, 'Found:', SIZE( charArrAlloc ),&
                      LBOUND( charArrAlloc ), UBOUND( charArrAlloc ),&
                      LEN( charArrAlloc )

10          FORMAT(A11,4(I3))

            WRITE(fmt, 20) SIZE( charArrAlloc ), LEN( charArrAlloc )
20          FORMAT("(",I2,"(A",I2,",','))")

            PRINT fmt, charArrAlloc


            IF (SIZE( charArrAlloc ) /= limits( 1 ))&
                                CALL zzrc( (failRC + 1_4) )
            IF (LBOUND(charArrAlloc, 1) /= limits( 2 ))&
                                CALL zzrc( (failRC + 2_4) )
            IF (UBOUND(charArrAlloc, 1) /= limits( 3 ))&
                                CALL zzrc( (failRC + 3_4) )
            IF (LEN( charArrAlloc ) /= limits( 4 ))&
                                CALL zzrc( (failRC + 4_4) )

        END SUBROUTINE CheckIt

END PROGRAM exprIsArraySection05
