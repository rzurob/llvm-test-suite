!*  ===================================================================
!*
!*                               CHARACTER Intrinsic Type
!*
!*  DATE                       : September 19, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is a
!*                               Deferred Length ALLOCATABLE Array Dummy
!*                               Argument of Type CHARACTER
!*  SECONDARY FUNCTIONS TESTED : The corresponding Actual Argument is an
!*                               Allocated/Unallocated ALLOCATABLE
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

    CHARACTER(:), ALLOCATABLE :: charArrAlloc( :,: )

    CONTAINS

        SUBROUTINE ModSubAssign( charArrArg )
            CHARACTER(:), ALLOCATABLE :: charArrArg( :,: )

            CHARACTER(5) :: char5Var( 5,5 ) =&
                RESHAPE((/ ('ChEcK', i = 1, 25) /), (/ 5,5 /))


            IF ( ALLOCATED( charArrArg ) )      CALL zzrc( 10_4 )

            charArrArg = char5Var( :5,:5 )( 3: ) // '-' //&
                         (char5Var // '-' // char5Var( :5,:5 )( :3 ))

            IF (.NOT. ALLOCATED( charArrArg ))  CALL zzrc( 11_4 )

        END SUBROUTINE ModSubAssign

        SUBROUTINE ModSubCheck( charArrArg )
            CHARACTER(:), ALLOCATABLE :: charArrArg( :,: )


            PRINT *, "ModSubCheck():  ", SIZE( charArrArg ),&
                      SIZE(charArrArg, 1), SIZE(charArrArg, 2)

            IF (SIZE( charArrArg ) /= 25) CALL zzrc( 20_4 )

            IF (SIZE(charArrArg, 1) /= 5) CALL zzrc( 21_4 )
            IF (SIZE(charArrArg, 2) /= 5) CALL zzrc( 22_4 )

            PRINT *, LEN( charArrArg( 1,1 ) )
            IF (LEN( charArrArg( 1,1 ) ) /= 13) CALL zzrc( 23_4 )

            PRINT "(5(A13,', '))", charArrArg
            IF (.NOT. ALL( (charArrArg == 'EcK-ChEcK-ChE') )) CALL zzrc( 24_4 )

        END SUBROUTINE ModSubCheck

END MODULE mModule


PROGRAM deferredLenDummyArg02
    USE mModule

    CALL ModSubAssign( charArrAlloc )
    CALL ModSubCheck( charArrAlloc )

    CALL SubAssign( charArrAlloc )
    CALL SubCheck( charArrAlloc )

    CONTAINS

        SUBROUTINE SubAssign( charArrArg )
            CHARACTER(:), ALLOCATABLE :: charArrArg( :,: )

            CHARACTER(5) :: char5Var( 5,5 ) =&
                RESHAPE((/ ('pRiNt', i = 1, 25) /), (/ 5,5 /))


            IF (.NOT. ALLOCATED( charArrArg ))  CALL zzrc( 31_4 )

            charArrArg = char5Var( :3,:3 ) // '-' //&
                         (char5Var( 2:4,2:4 ) // '-' // char5Var( 3:,3: ))

            IF (.NOT. ALLOCATED( charArrArg ))  CALL zzrc( 31_4 )

        END SUBROUTINE SubAssign

        SUBROUTINE SubCheck( charArrArg )
            CHARACTER(:), ALLOCATABLE :: charArrArg( :,: )


            PRINT *, "SubCheck():  ", SIZE( charArrArg ),&
                      SIZE(charArrArg, 1), SIZE(charArrArg, 2)

            IF (SIZE( charArrArg ) /= 9)  CALL zzrc( 40_4 )

            IF (SIZE(charArrArg, 1) /= 3) CALL zzrc( 41_4 )
            IF (SIZE(charArrArg, 2) /= 3) CALL zzrc( 42_4 )

            PRINT *, LEN( charArrArg( 1,1 ) )
            IF (LEN( charArrArg( 1,1 ) ) /= 17) CALL zzrc( 43_4 )

            PRINT "(3(A17,', '))", charArrArg
            IF (.NOT. ALL( (charArrArg == 'pRiNt-pRiNt-pRiNt') ))&
                                                    CALL zzrc( 44_4 )

        END SUBROUTINE SubCheck

END PROGRAM deferredLenDummyArg02
