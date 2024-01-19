!*  ===================================================================
!*
!*                               CHARACTER Intrinsic Type
!*
!*  DATE                       : September 13, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Scalar of Type CHARACTER
!*  SECONDARY FUNCTIONS TESTED : and expr is a (complex) CHARACTER Scalar
!*                               Expression of a smaller Size than variable
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
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM allocatedScalarCharExpr02

    CHARACTER(2) :: char2Var1 = 'ab'
    CHARACTER(2) :: char2Var2 = 'CD'

    CHARACTER(3) :: char3Var1 = 'uvw'
    CHARACTER(3) :: char3Var2 = 'XYZ'

    CHARACTER(6) :: char6Var1 = 'LMNopq'

    CHARACTER(7), ALLOCATABLE :: charScalarAlloc


    ALLOCATE( charScalarAlloc )

    CALL SubStrAssign( )
    CALL CatAssign( )
    CALL SubStrCatAssign( )
    CALL IntrinsicReturnAssign( )
    CALL MultipleCatAssign( )

    CONTAINS

        SUBROUTINE SubStrAssign( )

            IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 10_4 )

            charScalarAlloc = char6Var1( :5 )

            IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 11_4 )
            PRINT *, '1) charScalarAlloc = "', charScalarAlloc, '"'
            IF (charScalarAlloc /= 'LMNop  ')       CALL zzrc( 12_4 )

        END SUBROUTINE SubStrAssign


        SUBROUTINE CatAssign( )

            IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 20_4 )

            charScalarAlloc = char3Var1 // char2Var2

            IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 21_4 )
            PRINT *, '2) charScalarAlloc = "', charScalarAlloc, '"'
            IF (charScalarAlloc /= 'uvwCD  ')       CALL zzrc( 22_4 )

        END SUBROUTINE CatAssign


        SUBROUTINE SubStrCatAssign( )

            IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 30_4 )

            charScalarAlloc = char6Var1( :3 ) // char3Var2( 2: )

            IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 31_4 )
            PRINT *, '3) charScalarAlloc = "', charScalarAlloc, '"'
            IF (charScalarAlloc /= 'LMNYZ  ')       CALL zzrc( 32_4 )

        END SUBROUTINE SubStrCatAssign


        SUBROUTINE IntrinsicReturnAssign( )

            IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 40_4 )

            charScalarAlloc = REPEAT(char2Var2, 3)

            IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 41_4 )
            PRINT *, '4) charScalarAlloc = "', charScalarAlloc, '"'
            IF (charScalarAlloc /= 'CDCDCD ')       CALL zzrc( 42_4 )

        END SUBROUTINE IntrinsicReturnAssign


        SUBROUTINE MultipleCatAssign( )

            IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 50_4 )

            charScalarAlloc = char2Var1 // char3Var1( 2:2 ) //&
                                CHAR( 49 ) // char6Var1( 5: )

            IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 51_4 )
            PRINT *, '5) charScalarAlloc = "', charScalarAlloc, '"'
            IF (charScalarAlloc /= 'abv1pq ')       CALL zzrc( 52_4 )

        END SUBROUTINE MultipleCatAssign

END PROGRAM allocatedScalarCharExpr02
