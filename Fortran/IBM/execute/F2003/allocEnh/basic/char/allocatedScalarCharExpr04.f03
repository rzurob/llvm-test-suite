!*  ===================================================================
!*
!*                               CHARACTER Intrinsic Type
!*
!*  DATE                       : September 14, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Scalar of Type CHARACTER
!*                               with Defered Length
!*  SECONDARY FUNCTIONS TESTED : and expr is a (complex) CHARACTER Scalar
!*                               Expression of same/smaller/bigger Size than
!*                               variable
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
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM allocatedScalarCharExpr04

    CHARACTER(:), ALLOCATABLE :: scalarCharAlloc

    CHARACTER(2) :: char2Var1 = 'gh'
    CHARACTER(2) :: char2Var2 = 'IJ'

    CHARACTER(3) :: char3Var1 = 'qrs'
    CHARACTER(3) :: char3Var2 = 'tuv'

    CHARACTER(5) :: char5Var1 = 'RqPoN'


    ALLOCATE(CHARACTER(3) :: scalarCharAlloc)


    IF (.NOT. ALLOCATED( scalarCharAlloc )) CALL zzrc( 10_4 )
    scalarCharAlloc = char5Var1( 2:4 )

    IF (.NOT. ALLOCATED( scalarCharAlloc )) CALL zzrc( 11_4 )
    PRINT *, "1) scalarCharAlloc = '", scalarCharAlloc, "'"
    IF (LEN( scalarCharAlloc ) /= 3)        CALL zzrc( 12_4 )
    IF (scalarCharAlloc /= 'qPo')           CALL zzrc( 13_4 )


    IF (.NOT. ALLOCATED( scalarCharAlloc )) CALL zzrc( 20_4 )
    scalarCharAlloc = char3Var1( :2 ) // char2Var1( 2: )

    IF (.NOT. ALLOCATED( scalarCharAlloc )) CALL zzrc( 21_4 )
    PRINT *, "2) scalarCharAlloc = '", scalarCharAlloc, "'"
    IF (LEN( scalarCharAlloc ) /= 3)        CALL zzrc( 22_4 )
    IF (scalarCharAlloc /= 'qrh')           CALL zzrc( 23_4 )


    CALL BigSmallAssign( )

    CONTAINS

        SUBROUTINE BigSmallAssign( )

            IF (.NOT. ALLOCATED( scalarCharAlloc )) CALL zzrc( 30_4 )
            scalarCharAlloc = char2Var1 // char3Var2( 2: ) //&
                                char5Var1( 4: ) // char3Var2

            IF (.NOT. ALLOCATED( scalarCharAlloc )) CALL zzrc( 31_4 )
            PRINT *, "3) scalarCharAlloc = '", scalarCharAlloc, "'"
            IF (LEN( scalarCharAlloc ) /= 9)        CALL zzrc( 32_4 )
            IF (scalarCharAlloc /= 'ghuvoNtuv')     CALL zzrc( 33_4 )


            IF (.NOT. ALLOCATED( scalarCharAlloc )) CALL zzrc( 40_4 )
            scalarCharAlloc = REPEAT(char2Var2( :1 ), 2) // CHAR( 48 )&
                                // CHAR( 49 ) // REPEAT(char3Var2( :1 ), 2)

            IF (.NOT. ALLOCATED( scalarCharAlloc )) CALL zzrc( 41_4 )
            PRINT *, "4) scalarCharAlloc = '", scalarCharAlloc, "'"
            IF (LEN( scalarCharAlloc ) /= 6)        CALL zzrc( 42_4 )
            IF (scalarCharAlloc /= 'II01tt')        CALL zzrc( 43_4 )

        END SUBROUTINE BigSmallAssign

END PROGRAM allocatedScalarCharExpr04
