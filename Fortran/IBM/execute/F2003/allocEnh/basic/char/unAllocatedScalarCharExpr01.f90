!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : unAllocatedScalarCharExpr01 - Basic Tests:
!*                               CHARACTER Intrinsic Type
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : September 13, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Unallocated ALLOCATABLE Scalar of Type
!*                               CHARACTER
!*  SECONDARY FUNCTIONS TESTED : and expr is a (complex) CHARACTER Scalar
!*                               Expression of the same Size as variable
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

PROGRAM unAllocatedScalarCharExpr01

    CHARACTER(3), ALLOCATABLE :: charScalarAlloc

    CHARACTER(1) :: char1Var1 = 'M'
    CHARACTER(1) :: char1Var2 = 'N'

    CHARACTER(2) :: char2Var1 = 'gh'

    CHARACTER(3) :: char3Var1 = 'XYZ'
    CHARACTER(3) :: char3Var2 = 'abc'

    CHARACTER(6) :: char6Var1 = 'a1B2c3'


!
!  1)  Sub-String
!
    IF ( ALLOCATED( charScalarAlloc ) )     CALL zzrc( 10_4 )
    charScalarAlloc = char6Var1( 3:5 )

    IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 11_4 )
    PRINT *, " 1) char6Var1( 3:5 ) == '",&
                  char6Var1( 3:5 ), "' == '", charScalarAlloc, "'"

    IF (charScalarAlloc /= 'B2c')           CALL zzrc( 12_4 )
    DEALLOCATE( charScalarAlloc )


!
!  2)  String Concatenation
!
    IF ( ALLOCATED( charScalarAlloc ) )     CALL zzrc( 20_4 )
    charScalarAlloc = char1Var1 // char2Var1

    IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 21_4 )
    PRINT *, " 2) char1Var1 // char2Var1 == '",&
              char1Var1, "' // '", char1Var1, "' == '",&
                 (char1Var1 // char2Var1), "' == '", charScalarAlloc, "'"

    IF (charScalarAlloc /= 'Mgh')           CALL zzrc( 22_4 )
    DEALLOCATE( charScalarAlloc )


!
!  3)  Sub-String Concatenation
!
    IF ( ALLOCATED( charScalarAlloc ) )     CALL zzrc( 30_4 )
    charScalarAlloc = char2Var1( :1 ) // char3Var1( 2: )

    IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 31_4 )
    PRINT *, " 3) char2Var1( :1 ) // char3Var1( 2: ) == '",&
                  char2Var1( :1 ), "' // '", char3Var1( 2: ), "' == '",&
                  (char2Var1( :1 ) // char3Var1( 2: )), "' == '",&
                  charScalarAlloc, "'"

    IF (charScalarAlloc /= 'gYZ')           CALL zzrc( 32_4 )
    DEALLOCATE( charScalarAlloc )


!
!  4)  Intrinsic Function Return Value
!
    IF ( ALLOCATED( charScalarAlloc ) )     CALL zzrc( 40_4 )
    charScalarAlloc = REPEAT(char1Var2, 3)

    IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 41_4 )
    PRINT *, " 4) REPEAT(char1Var2, 3) == REPEAT(", char1Var2, ", 3) == '",&
                  REPEAT(char1Var2, 3), "' == '", charScalarAlloc, "'"

    IF (charScalarAlloc /= 'NNN')           CALL zzrc( 42_4 )
    DEALLOCATE( charScalarAlloc )



!
!  5)  Concatenation of Intrinsic Function Return Values
!
    IF ( ALLOCATED( charScalarAlloc ) )     CALL zzrc( 50_4 )
    charScalarAlloc = CHAR( 48 ) // REPEAT(char1Var1, 2)

    IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 51_4 )
    PRINT *, " 5) CHAR( 48 ) // REPEAT(char1Var1, 2) == ",&
                 "CHAR( 48 ) // REPEAT(", char1Var1, ", 2)"
    PRINT *, "    == '", CHAR( 48 ), "' // '", REPEAT(char1Var1, 2),&
               "' == '", (CHAR( 48 ) // REPEAT(char1Var1, 2)),&
               "' == '", charScalarAlloc, "'"

    IF (charScalarAlloc /= '0MM')           CALL zzrc( 52_4 )
    DEALLOCATE( charScalarAlloc )



!
!  6)  Concatenation of 3 Values
!
    IF ( ALLOCATED( charScalarAlloc ) )     CALL zzrc( 60_4 )
    charScalarAlloc = char1Var1 // CHAR( 50 ) // char2Var1( 2: )

    IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 61_4 )
    PRINT *, " 6) char1Var1 // CHAR( 50 ) // char2Var1( 2: ) == '",&
                 char1Var1, "' // '", CHAR( 50 ), "' // '",&
                    char2Var1( 2: ), "'"
    PRINT *, "    == '", (char1Var1 // CHAR( 50 ) // char2Var1( 2: )),&
               "' == '", charScalarAlloc, "'"

    IF (charScalarAlloc /= 'M2h')           CALL zzrc( 62_4 )
    DEALLOCATE( charScalarAlloc )

END PROGRAM unAllocatedScalarCharExpr01
