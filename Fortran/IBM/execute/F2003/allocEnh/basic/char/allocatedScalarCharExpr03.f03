!*  ===================================================================
!*
!*                               CHARACTER Intrinsic Type
!*
!*  DATE                       : September 13, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Scalar of Type CHARACTER
!*  SECONDARY FUNCTIONS TESTED : and expr is a (complex) CHARACTER Scalar
!*                               Expression of a bigger Size than variable
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

MODULE mModule

    CHARACTER(2) :: char2Var1 = 'ZY'
    CHARACTER(2) :: char2Var2 = 'xw'

    CHARACTER(4) :: char4Var2 = 'DCBA'

    CHARACTER(5) :: char5Var1 = 'LmNoP'

    CHARACTER(3), ALLOCATABLE :: charScalarAlloc

    CONTAINS

        SUBROUTINE SubStrCatAssign( )

            IF (.NOT. ALLOCATED( charScalarAlloc )) ERROR STOP 30_4
            charScalarAlloc = char4Var2( 2:3 ) // char5Var1( 2:3 )

            IF (.NOT. ALLOCATED( charScalarAlloc )) ERROR STOP 31_4

            PRINT *, '3) charScalarAlloc = "', charScalarAlloc, '"'
            IF (charScalarAlloc /= 'CBm')           ERROR STOP 32_4

        END SUBROUTINE SubStrCatAssign

END MODULE mModule


PROGRAM allocatedScalarCharExpr03
    USE mModule


    ALLOCATE( charScalarAlloc )


    CALL SubStrAssign( )
    CALL StrCatAssign( )
    CALL SubStrCatAssign( )
    CALL IntrinsicReturnAssign( )


    IF (.NOT. ALLOCATED( charScalarAlloc )) ERROR STOP 50_4
    charScalarAlloc = char2Var1( 2: ) // CHAR( 51 ) // char2Var2

    IF (.NOT. ALLOCATED( charScalarAlloc )) ERROR STOP 51_4

    PRINT *, '5) charScalarAlloc = "', charScalarAlloc, '"'
    IF (charScalarAlloc /= 'Y3x')           ERROR STOP 52_4


    CONTAINS

        SUBROUTINE IntrinsicReturnAssign( )

            IF (.NOT. ALLOCATED( charScalarAlloc )) ERROR STOP 40_4
            charScalarAlloc = REPEAT(char2Var1, 2)

            IF (.NOT. ALLOCATED( charScalarAlloc )) ERROR STOP 41_4

            PRINT *, '4) charScalarAlloc = "', charScalarAlloc, '"'
            IF (charScalarAlloc /= 'ZYZ')           ERROR STOP 42_4

        END SUBROUTINE IntrinsicReturnAssign

END PROGRAM allocatedScalarCharExpr03


SUBROUTINE SubStrAssign( )
    USE mModule

    IF (.NOT. ALLOCATED( charScalarAlloc )) ERROR STOP 10_4
    charScalarAlloc = char5Var1( 2: )

    IF (.NOT. ALLOCATED( charScalarAlloc )) ERROR STOP 11_4

    PRINT *, '1) charScalarAlloc = "', charScalarAlloc, '"'
    IF (charScalarAlloc /= 'mNo')           ERROR STOP 12_4

END SUBROUTINE SubStrAssign


SUBROUTINE StrCatAssign( )
    USE mModule

    IF (.NOT. ALLOCATED( charScalarAlloc )) ERROR STOP 20_4
    charScalarAlloc = char2Var1 // char2Var2

    IF (.NOT. ALLOCATED( charScalarAlloc )) ERROR STOP 21_4

    PRINT *, '2) charScalarAlloc = "', charScalarAlloc, '"'
    IF (charScalarAlloc /= 'ZYx')           ERROR STOP 22_4

END SUBROUTINE StrCatAssign
