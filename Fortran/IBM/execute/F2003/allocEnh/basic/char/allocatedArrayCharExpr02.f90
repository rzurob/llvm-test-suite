!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : allocatedArrayCharExpr02 - Basic Tests:
!*                               CHARACTER Intrinsic Type
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : September 18, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is
!*                               an Allocated ALLOCATABLE Array of Type
!*                               CHARACTER
!*  SECONDARY FUNCTIONS TESTED : and expr is a (complex) CHARACTER Array
!*                               Expression of a smaller Size than variable
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
    CHARACTER(:), ALLOCATABLE :: charArrayAlloc( :,: )
END MODULE mModule


PROGRAM allocatedArrayCharExpr02
    USE mModule


    ALLOCATE(CHARACTER(6) :: charArrayAlloc( 5,5 ))

    IF (.NOT. ALLOCATED( charArrayAlloc ))  CALL zzrc( 10_4 )


    CALL Assign( )


    IF (.NOT. ALLOCATED( charArrayAlloc ))  CALL zzrc( 20_4 )

    PRINT *, SIZE( charArrayAlloc ),&
            SIZE(charArrayAlloc, 1), SIZE(charArrayAlloc, 2)

    IF (SIZE( charArrayAlloc ) /= 9)        CALL zzrc( 30_4 )
    IF (SIZE(charArrayAlloc, 1) /= 3)       CALL zzrc( 31_4 )
    IF (SIZE(charArrayAlloc, 2) /= 3)       CALL zzrc( 32_4 )


    DO i = 1, 3
        DO j = 1, 3
            PRINT *,"charArrayAlloc(",j,",",i,") == (",&
                    charArrayAlloc( j,i ),")",LEN( charArrayAlloc( j,i ) )

            IF (LEN( charArrayAlloc( j,i ) ) /= 3)  CALL zzrc( 40_4 )
            IF (charArrayAlloc( j,i ) /= 'bMX')     CALL zzrc( 41_4 )
        END DO
    END DO

END PROGRAM allocatedArrayCharExpr02


SUBROUTINE Assign( )
    USE mModule

    CHARACTER(1) :: char1Arr( 3,3 ) = 'M'

    CHARACTER(3) :: char3Var1 = 'XYZ'
    CHARACTER(3) :: char3Var2( 3,3 ) = 'cba'


    charArrayAlloc = char3Var2( 1:3,1:3 )( 2:2 )  //  char1Arr  //&
                RESHAPE((/ (char3Var1( 1:1 ), i = 1, 9) /), (/ 3,3 /))

END SUBROUTINE Assign
