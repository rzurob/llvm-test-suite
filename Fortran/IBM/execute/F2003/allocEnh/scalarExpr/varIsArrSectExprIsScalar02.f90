!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : varIsArrSectExprIsScalar02 - expr is a
!*                               Scalar and variable is an Array
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : October 31, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Array Section from an Allocated ALLOCATABLE
!*                               Array of CHARACTER,
!*  SECONDARY FUNCTIONS TESTED : and expr is a Scalar (also of Type CHARACTER)
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 8
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
!*  If expr is a scalar and variable is an array, the expr is treated as if
!*  it were an array of the same shape as variable with every element of the
!*  array equal to the scalar value of expr.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM varIsArrSectExprIsScalar02

    CHARACTER(2) :: charArr( 10 ) =&
        [ 'ab', 'CD', 'ef', 'GH', 'ij', 'KL', 'mn', 'OP', 'qr', 'ST' ]

    CHARACTER(:), POINTER :: charArrPtr( : )

    CHARACTER(:), ALLOCATABLE, TARGET :: defArrAlloc( : )
    CHARACTER(2), ALLOCATABLE, TARGET :: charArrAlloc( : )


    charArrAlloc = charArr
    charArrPtr => charArrAlloc

    CALL Dump(charArr, 10_4)


    charArrAlloc( 4:5 ) = 'XX'
    CALL Dump([ charArr( :3 ), ('XX', i = 1, 2), charArr( 6: ) ], 20_4)

    charArrAlloc( 2:4 ) = charArrAlloc( 3 )
    CALL Dump([charArr( 1 ),(charArr( 3 ), i = 1, 3),'XX',charArr( 6: )], 30_4)

    charArrAlloc( 2:5 ) = charArrAlloc( 3 )( 2: )
    CALL Dump([ charArr( 1 ), ((charArr( 3 )( 2: ) // ' '), i = 1, 4),&
                                                    charArr( 6: )], 40_4)

    charArrAlloc( :5 ) = charArrAlloc( 3 )( :1 ) // charArrAlloc( 1 )
    CALL Dump([&
        ((charArr( 3 )( 2: ) // charArr( 1 )( :1 )), i = 1, 5),&
                                                charArr( 6: )], 50_4)


    defArrAlloc = charArr
    charArrPtr => defArrAlloc

    CALL Dump(charArr, 110_4)


    defArrAlloc( 4:5 ) = 'XX'
    CALL Dump([ charArr( :3 ), ('XX', i = 1, 2), charArr( 6: ) ], 120_4)

    defArrAlloc( 2:4 ) = defArrAlloc( 3 )
    CALL Dump([charArr( 1 ),(charArr( 3 ), i = 1, 3),'XX',charArr( 6: )],130_4)

    defArrAlloc( 2:5 ) = defArrAlloc( 3 )( 2: )
    CALL Dump([ charArr( 1 ), ((charArr( 3 )( 2: ) // ' '), i = 1, 4),&
                                                    charArr( 6: )], 140_4)

    defArrAlloc( :5 ) = defArrAlloc( 3 )( :1 ) // defArrAlloc( 1 )
    CALL Dump([&
        ((charArr( 3 )( 2: ) // charArr( 1 )( :1 )), i = 1, 5),&
                                                charArr( 6: )], 150_4)


    CONTAINS


        SUBROUTINE Dump(p, failRC)
            CHARACTER(*) :: p( : )
            INTEGER(4) :: failRC


            IF ( ASSOCIATED(charArrPtr, charArrAlloc) ) THEN
                IF (.NOT. ALLOCATED( charArrAlloc ))CALL zzrc( (failRC + 1_4) )

            ELSE IF ( ASSOCIATED(charArrPtr, defArrAlloc) ) THEN
                IF (.NOT. ALLOCATED( defArrAlloc )) CALL zzrc( (failRC + 2_4) )

            ELSE
                CALL zzrc( ( failRC + 3_4) )
            END IF


            PRINT *
            PRINT 10, failRC, LEN( charArrPtr ), SIZE( charArrPtr )
            PRINT 20, p
            PRINT 20, charArrPtr
10          FORMAT(I3,I2,I3)
20          FORMAT('    (',9('"',A2,'",'),'"',A2,'")') 


            IF (LEN( charArrPtr ) /= 2)       CALL zzrc( (failRC + 4_4) )
            IF (SIZE( charArrPtr ) /= 10)     CALL zzrc( (failRC + 5_4) )
            IF ( ANY( (charArrPtr /= p) ) )   CALL zzrc( (failRC + 6_4) )

        END SUBROUTINE Dump

END PROGRAM varIsArrSectExprIsScalar02
