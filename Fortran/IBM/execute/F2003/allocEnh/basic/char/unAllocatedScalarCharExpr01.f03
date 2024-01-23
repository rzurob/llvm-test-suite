!*  ===================================================================
!*
!*                               CHARACTER Intrinsic Type
!*
!*  DATE                       : September 13, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Unallocated ALLOCATABLE Scalar of Type
!*                               CHARACTER
!*  SECONDARY FUNCTIONS TESTED : and expr is a (complex) CHARACTER Scalar
!*                               Expression of the same Size as variable
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
    IF ( ALLOCATED( charScalarAlloc ) )     ERROR STOP 10_4
    charScalarAlloc = char6Var1( 3:5 )

    IF (.NOT. ALLOCATED( charScalarAlloc )) ERROR STOP 11_4
    PRINT *, " 1) char6Var1( 3:5 ) == '",&
                  char6Var1( 3:5 ), "' == '", charScalarAlloc, "'"

    IF (charScalarAlloc /= 'B2c')           ERROR STOP 12_4
    DEALLOCATE( charScalarAlloc )


!
!  2)  String Concatenation
!
    IF ( ALLOCATED( charScalarAlloc ) )     ERROR STOP 20_4
    charScalarAlloc = char1Var1 // char2Var1

    IF (.NOT. ALLOCATED( charScalarAlloc )) ERROR STOP 21_4
    PRINT *, " 2) char1Var1 // char2Var1 == '",&
              char1Var1, "' // '", char1Var1, "' == '",&
                 (char1Var1 // char2Var1), "' == '", charScalarAlloc, "'"

    IF (charScalarAlloc /= 'Mgh')           ERROR STOP 22_4
    DEALLOCATE( charScalarAlloc )


!
!  3)  Sub-String Concatenation
!
    IF ( ALLOCATED( charScalarAlloc ) )     ERROR STOP 30_4
    charScalarAlloc = char2Var1( :1 ) // char3Var1( 2: )

    IF (.NOT. ALLOCATED( charScalarAlloc )) ERROR STOP 31_4
    PRINT *, " 3) char2Var1( :1 ) // char3Var1( 2: ) == '",&
                  char2Var1( :1 ), "' // '", char3Var1( 2: ), "' == '",&
                  (char2Var1( :1 ) // char3Var1( 2: )), "' == '",&
                  charScalarAlloc, "'"

    IF (charScalarAlloc /= 'gYZ')           ERROR STOP 32_4
    DEALLOCATE( charScalarAlloc )


!
!  4)  Intrinsic Function Return Value
!
    IF ( ALLOCATED( charScalarAlloc ) )     ERROR STOP 40_4
    charScalarAlloc = REPEAT(char1Var2, 3)

    IF (.NOT. ALLOCATED( charScalarAlloc )) ERROR STOP 41_4
    PRINT *, " 4) REPEAT(char1Var2, 3) == REPEAT(", char1Var2, ", 3) == '",&
                  REPEAT(char1Var2, 3), "' == '", charScalarAlloc, "'"

    IF (charScalarAlloc /= 'NNN')           ERROR STOP 42_4
    DEALLOCATE( charScalarAlloc )



!
!  5)  Concatenation of Intrinsic Function Return Values
!
    IF ( ALLOCATED( charScalarAlloc ) )     ERROR STOP 50_4
    charScalarAlloc = CHAR( 48 ) // REPEAT(char1Var1, 2)

    IF (.NOT. ALLOCATED( charScalarAlloc )) ERROR STOP 51_4
    PRINT *, " 5) CHAR( 48 ) // REPEAT(char1Var1, 2) == ",&
                 "CHAR( 48 ) // REPEAT(", char1Var1, ", 2)"
    PRINT *, "    == '", CHAR( 48 ), "' // '", REPEAT(char1Var1, 2),&
               "' == '", (CHAR( 48 ) // REPEAT(char1Var1, 2)),&
               "' == '", charScalarAlloc, "'"

    IF (charScalarAlloc /= '0MM')           ERROR STOP 52_4
    DEALLOCATE( charScalarAlloc )



!
!  6)  Concatenation of 3 Values
!
    IF ( ALLOCATED( charScalarAlloc ) )     ERROR STOP 60_4
    charScalarAlloc = char1Var1 // CHAR( 50 ) // char2Var1( 2: )

    IF (.NOT. ALLOCATED( charScalarAlloc )) ERROR STOP 61_4
    PRINT *, " 6) char1Var1 // CHAR( 50 ) // char2Var1( 2: ) == '",&
                 char1Var1, "' // '", CHAR( 50 ), "' // '",&
                    char2Var1( 2: ), "'"
    PRINT *, "    == '", (char1Var1 // CHAR( 50 ) // char2Var1( 2: )),&
               "' == '", charScalarAlloc, "'"

    IF (charScalarAlloc /= 'M2h')           ERROR STOP 62_4
    DEALLOCATE( charScalarAlloc )

END PROGRAM unAllocatedScalarCharExpr01
