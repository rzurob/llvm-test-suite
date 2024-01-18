!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : allocatedScalarCharExpr03 - Basic Tests:
!*                               CHARACTER Intrinsic Type
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : September 13, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Scalar of Type CHARACTER
!*  SECONDARY FUNCTIONS TESTED : and expr is a (complex) CHARACTER Scalar
!*                               Expression of a bigger Size than variable
!*
!*  DRIVER STANZA              : xlf2003
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

            IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 30_4 )
            charScalarAlloc = char4Var2( 2:3 ) // char5Var1( 2:3 )

            IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 31_4 )

            PRINT *, '3) charScalarAlloc = "', charScalarAlloc, '"'
            IF (charScalarAlloc /= 'CBm')           CALL zzrc( 32_4 )

        END SUBROUTINE SubStrCatAssign

END MODULE mModule


PROGRAM allocatedScalarCharExpr03
    USE mModule


    ALLOCATE( charScalarAlloc )


    CALL SubStrAssign( )
    CALL StrCatAssign( )
    CALL SubStrCatAssign( )
    CALL IntrinsicReturnAssign( )


    IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 50_4 )
    charScalarAlloc = char2Var1( 2: ) // CHAR( 51 ) // char2Var2

    IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 51_4 )

    PRINT *, '5) charScalarAlloc = "', charScalarAlloc, '"'
    IF (charScalarAlloc /= 'Y3x')           CALL zzrc( 52_4 )


    CONTAINS

        SUBROUTINE IntrinsicReturnAssign( )

            IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 40_4 )
            charScalarAlloc = REPEAT(char2Var1, 2)

            IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 41_4 )

            PRINT *, '4) charScalarAlloc = "', charScalarAlloc, '"'
            IF (charScalarAlloc /= 'ZYZ')           CALL zzrc( 42_4 )

        END SUBROUTINE IntrinsicReturnAssign

END PROGRAM allocatedScalarCharExpr03


SUBROUTINE SubStrAssign( )
    USE mModule

    IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 10_4 )
    charScalarAlloc = char5Var1( 2: )

    IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 11_4 )

    PRINT *, '1) charScalarAlloc = "', charScalarAlloc, '"'
    IF (charScalarAlloc /= 'mNo')           CALL zzrc( 12_4 )

END SUBROUTINE SubStrAssign


SUBROUTINE StrCatAssign( )
    USE mModule

    IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 20_4 )
    charScalarAlloc = char2Var1 // char2Var2

    IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 21_4 )

    PRINT *, '2) charScalarAlloc = "', charScalarAlloc, '"'
    IF (charScalarAlloc /= 'ZYx')           CALL zzrc( 22_4 )

END SUBROUTINE StrCatAssign
