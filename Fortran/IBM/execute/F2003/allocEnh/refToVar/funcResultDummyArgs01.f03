!*  ===================================================================
!*
!*                               expr Contain References to variable
!*
!*  DATE                       : October 17, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Intrinsic
!*                               Type and expr calls a User Defined FUNCTION
!*                               that has variable as the Actual Argument.
!*  SECONDARY FUNCTIONS TESTED : Elements of the corresponding Dummy Argument
!*                               are Assigned to the FUNCTION's Result.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 5
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
!*  Both variable and expr may contain references to any portion of variable.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM funcResultDummyArgs01

    INTEGER :: intArr( 10,10 ) =&
        RESHAPE((/ ((((i * 10) + j), j = 0, 9), i = 0, 9) /), (/ 10,10 /))

    INTEGER, ALLOCATABLE :: intArrAlloc( :,: )


    intArrAlloc = intArr
    CALL CheckIt(10, 10, intArr, 10_4)


    intArrAlloc = AssignIt(intArrAlloc, 0)
    CALL CheckIt(10, 10, intArr, 20_4)

    intArrAlloc = AssignIt(intArrAlloc, 1)
    CALL CheckIt(10, 6, intArr( :,3:8 ), 30_4)

    intArrAlloc = AssignIt(intArrAlloc, 2)
    CALL CheckIt(10, 12, intArr( :,(/ 3,4,5,6,7,8,3,4,5,6,7,8 /) ), 40_4)

    intArrAlloc = AssignIt(intArrAlloc, 3)
    CALL CheckIt(6, 12, intArr( 3:8,(/ 3,4,5,6,7,8,3,4,5,6,7,8 /) ), 50_4)

    intArrAlloc = AssignIt(intArrAlloc, 4)
    CALL CheckIt(6, 6, intArr( 3:8,3:8 ), 60_4)


    CONTAINS


        FUNCTION AssignIt(dummyIntArg, test)
            INTEGER :: dummyIntArg( :,: )
            INTEGER :: test

            INTEGER, ALLOCATABLE :: AssignIt( :,: )


            IF (test == 0) THEN
                AssignIt = dummyIntArg

            ELSE IF (test == 1) THEN
                AssignIt = dummyIntArg( :,3:8 )

            ELSE IF (test == 2) THEN
                AssignIt = dummyIntArg( :,(/ ((MOD(i, 6) + 1), i = 0, 11) /) )

            ELSE IF (test == 3) THEN
                AssignIt = dummyIntArg( 3:8,: )

            ELSE IF (test == 4) THEN
                AssignIt = dummyIntArg( :,7:12 )
            END IF

        END FUNCTION AssignIt


        SUBROUTINE CheckIt(size1, size2, iA, failRC)
            INTEGER :: size1
            INTEGER :: size2
            INTEGER :: iA( :,: )
            INTEGER(4) :: failRC


            IF (.NOT. ALLOCATED( intArrAlloc )) CALL zzrc( failRC )


            PRINT *
            PRINT *, (failRC / 10), size1, SIZE(intArrAlloc, 1),&
                                    size2, SIZE(intArrAlloc, 2)

            DO i = 1, SIZE(intArrAlloc, 1)
                PRINT *, intArrAlloc( i,: )
            END DO


            IF (SIZE(intArrAlloc, 1) /= size1)  CALL zzrc( (failRC + 1_4) )
            IF (SIZE(intArrAlloc, 2) /= size2)  CALL zzrc( (failRC + 2_4) )

            IF ( ANY( (intArrAlloc /= iA) ))    CALL zzrc( (failRC + 3_4) )

        END SUBROUTINE CheckIt

END PROGRAM funcResultDummyArgs01
