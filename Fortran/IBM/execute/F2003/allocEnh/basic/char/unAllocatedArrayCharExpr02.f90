!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : unAllocatedArrayCharExpr02 - Basic Tests:
!*                               CHARACTER Intrinsic Type
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : September 18, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is
!*                               an Unallocated ALLOCATABLE Array of Type
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

    CHARACTER(7), ALLOCATABLE :: charArrAlloc( : )

    CONTAINS

        SUBROUTINE AssignSub( char3Var1 )
            CHARACTER(3), INTENT(IN) :: char3Var1
            
            CHARACTER(3) :: char3Var2( 9 ) = (/ ('thx', i = 1, 9) /)


            IF ( ALLOCATED( charArrAlloc ) )        CALL zzrc( 10_4 )

            charArrAlloc = char3Var2 // char3Var1

            IF (.NOT. ALLOCATED( charArrAlloc ))    CALL zzrc( 11_4 )

        END SUBROUTINE AssignSub

END MODULE mModule


PROGRAM unAllocatedArrayCharExpr02
    USE mModule

    INTEGER(4) :: i


    CALL AssignSub( '113' )


    PRINT *, SIZE( charArrAlloc )
    IF (SIZE( charArrAlloc ) /= 9) CALL zzrc( 21_4 )

    DO i = 1, 9
        PRINT *, i, LEN( charArrAlloc( i ) ), "'", charArrAlloc( i ), "'"

        IF (LEN( charArrAlloc( i ) ) /= 7)  CALL zzrc( (30_4 + i) )
        IF (charArrAlloc( i ) /= 'thx113 ') CALL zzrc( (40_4 + i) )
    END DO

END PROGRAM unAllocatedArrayCharExpr02
