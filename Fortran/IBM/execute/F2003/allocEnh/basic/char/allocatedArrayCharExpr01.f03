!*  ===================================================================
!*
!*                               CHARACTER Intrinsic Type
!*
!*  DATE                       : September 15, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is
!*                               an Allocated ALLOCATABLE Array of Type
!*                               CHARACTER
!*  SECONDARY FUNCTIONS TESTED : and expr is a (complex) CHARACTER Array
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

PROGRAM allocatedArrayCharExpr01

        CHARACTER(:), ALLOCATABLE :: charArrayAlloc( : )

        CHARACTER(4) :: char4Var1 = 'zyxw'
        CHARACTER(4) :: char4Var2 = 'VUTS'

        CHARACTER(6) :: char6Var1 = 'abcdef'


        ALLOCATE(CHARACTER( 5 ) :: charArrayAlloc( 7 ))


        IF (.NOT. ALLOCATED( charArrayAlloc ))  CALL zzrc( 10_4 )
        charArrayAlloc = VarCharFunction(char6Var1( :5 ), 5)

        IF (.NOT. ALLOCATED( charArrayAlloc ))  CALL zzrc( 11_4 )
        PRINT *, "1) ", SIZE( charArrayAlloc ), LEN( charArrayAlloc( 1 ) )
        PRINT '("     charArrayAlloc = (",4(A5,","),A5,")")', charArrayAlloc

        IF (SIZE( charArrayAlloc ) /= 5)        CALL zzrc( 12_4 )
        IF (LEN( charArrayAlloc( 1 ) ) /= 5)    CALL zzrc( 13_4 )

        IF (.NOT. ALL(charArrayAlloc == 'abcde')) CALL zzrc( 14_4 )


        IF (.NOT. ALLOCATED( charArrayAlloc ))  CALL zzrc( 20_4 )
        charArrayAlloc = VarCharFunction(char4Var1( :3 ), 9) //&
                                (/ (char4Var2( 3: ), i = 1, 9) /)

        IF (.NOT. ALLOCATED( charArrayAlloc ))  CALL zzrc( 21_4 )
        PRINT *, "2) ", SIZE( charArrayAlloc ), LEN( charArrayAlloc( 1 ) )
        PRINT '("     charArrayAlloc = (",8(A5,","),A5,")")', charArrayAlloc

        IF (SIZE( charArrayAlloc ) /= 9)        CALL zzrc( 22_4 )
        IF (LEN( charArrayAlloc( 1 ) ) /= 5)    CALL zzrc( 23_4 )

        IF (.NOT. ALL(charArrayAlloc == 'zyxTS')) CALL zzrc( 24_4 )


        IF (.NOT. ALLOCATED( charArrayAlloc ))  CALL zzrc( 30_4 )
        charArrayAlloc = VarCharFunction(char6Var1( 3:4 ), 3) //&
                         VarCharFunction(char4Var1( 2:3 ), 3) //&
                         VarCharFunction(char4Var2( 2:2 ), 3)

        IF (.NOT. ALLOCATED( charArrayAlloc ))  CALL zzrc( 31_4 )
        PRINT *, "3) ", SIZE( charArrayAlloc ), LEN( charArrayAlloc( 1 ) )
        PRINT '("     charArrayAlloc = (",2(A5,","),A5,")")', charArrayAlloc

        IF (SIZE( charArrayAlloc ) /= 3)        CALL zzrc( 32_4 )
        IF (LEN( charArrayAlloc( 1 ) ) /= 5)    CALL zzrc( 33_4 )

        IF (.NOT. ALL(charArrayAlloc == 'cdyxU')) CALL zzrc( 34_4 )

    CONTAINS

        FUNCTION VarCharFunction(chr, n)
            CHARACTER(*), INTENT(in) :: chr
            INTEGER, INTENT(in) :: n

            CHARACTER(:), ALLOCATABLE :: VarCharFunction( : )

            VarCharFunction = (/ (chr, i = 1, n) /)

        END FUNCTION VarCharFunction

END PROGRAM allocatedArrayCharExpr01
