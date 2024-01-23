!*  ===================================================================
!*
!*                               CHARACTER Intrinsic Type
!*
!*  DATE                       : September 18, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is
!*                               an Allocated ALLOCATABLE Array of Type
!*                               CHARACTER
!*  SECONDARY FUNCTIONS TESTED : and expr is a (complex) CHARACTER Array
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

PROGRAM allocatedArrayCharExpr03

    CHARACTER(:), ALLOCATABLE :: charArrAlloc( :,:,: )

    ALLOCATE(CHARACTER(3) :: charArrAlloc( 3,3,3 ))


    IF (.NOT. ALLOCATED( charArrAlloc )) ERROR STOP 10_4
    charArrAlloc = RESHAPE((/ ('aXb', i = 1, 27) /), (/ 3,3,3 /))


    CALL AssignIt( charArrAlloc )


    IF (.NOT. ALLOCATED( charArrAlloc )) ERROR STOP 20_4

    PRINT *, SIZE( charArrAlloc ), SIZE(charArrAlloc, 1),&
                SIZE(charArrAlloc, 2), SIZE(charArrAlloc, 3)


    IF (SIZE( charArrAlloc ) /= 27) ERROR STOP 30_4

    IF (SIZE(charArrAlloc, 1) /= 9) ERROR STOP 31_4
    IF (SIZE(charArrAlloc, 2) /= 3) ERROR STOP 32_4
    IF (SIZE(charArrAlloc, 3) /= 1) ERROR STOP 33_4


    DO j = 1, 3
        DO k = 1, 9
            PRINT *, "charArrAlloc(",k,",",j,", 1 ) = '",&
                      charArrAlloc( k,j,1 ),"'",LEN( charArrAlloc( k,j,1 ) )

            IF (LEN( charArrAlloc( k,j,1 ) ) /= 6) ERROR STOP 41_4
            IF (charArrAlloc( k,j,1 ) /= 'aXbaXb') ERROR STOP 42_4
        END DO
    END DO


    CONTAINS


        SUBROUTINE AssignIt( arr )
            CHARACTER(*) :: arr( :,:,: )


            charArrAlloc =&
                RESHAPE(arr, (/ 9,3,1 /)) // RESHAPE(arr, (/ 9,3,1 /))

        END SUBROUTINE AssignIt

END PROGRAM allocatedArrayCharExpr03
