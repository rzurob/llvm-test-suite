!*  ===================================================================
!*
!*                               CHARACTER Intrinsic Type
!*
!*  DATE                       : September 20, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Assumed Length ALLOCATABLE Scalar Dummy
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

PROGRAM assumedDummyArg01

    CHARACTER(9), ALLOCATABLE :: charScalarAlloc


    IF ( ALLOCATED( charScalarAlloc ) )     ERROR STOP 10_4


    CALL Assign(charScalarAlloc, 0)

    IF (.NOT. ALLOCATED( charScalarAlloc )) ERROR STOP 20_4

    PRINT *, LEN( charScalarAlloc ), "'", charScalarAlloc, "'"

    IF (LEN_TRIM( charScalarAlloc ) /= 9)   ERROR STOP 21_4
    IF (charScalarAlloc /= 'FindRunTe')     ERROR STOP 22_4


    CALL Assign(charScalarAlloc, 1)

    IF (.NOT. ALLOCATED( charScalarAlloc )) ERROR STOP 30_4

    PRINT *, LEN( charScalarAlloc ), "'", charScalarAlloc, "'"

    IF (LEN_TRIM( charScalarAlloc ) /= 7)   ERROR STOP 31_4
    IF (charScalarAlloc /= 'ToBeRun')       ERROR STOP 32_4


    CONTAINS


        SUBROUTINE Assign(charScalarArg, testNum)
            CHARACTER(*), ALLOCATABLE :: charScalarArg
            INTEGER :: testNum

            CHARACTER(7) :: char7Var = 'FindRun'
            CHARACTER(8) :: char8Var = 'TestToBe'


            IF (testNum == 0) THEN
                charScalarArg = char7Var // char8Var

            ELSE
                charScalarArg = char8Var( 5: ) // char7Var( 5: )
            END IF

        END SUBROUTINE Assign

END PROGRAM assumedDummyArg01
