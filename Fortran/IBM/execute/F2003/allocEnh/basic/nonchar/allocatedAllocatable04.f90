!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : allocatedAllocatable04 - Basic Tests:
!*                               Non-CHARACTER Intrinsic Types
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : July 28, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of (Non-CHARACTER)
!*                               Intrinsic Type
!*  SECONDARY FUNCTIONS TESTED : with both different Shape and Length Type
!*                               Parameters from expr
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

PROGRAM allocatedAllocatable04

    REAL(16), ALLOCATABLE :: realArrayAlloc( :,:,: )


    ALLOCATE(realArrayAlloc( -5:-1,5,6:10 ),&
            SOURCE=RESHAPE((/ ((i / 3.14_16), i = 1, 125) /), (/ 5,5,5 /)))

    IF ( .NOT. ALLOCATED( realArrayAlloc )) CALL zzrc( 10_4 )

    IF (SIZE( realArrayAlloc ) /= 125) CALL zzrc( 20_4 )

    IF (SIZE(realArrayAlloc, 1) /= 5) CALL zzrc( 30_4 )
    IF (SIZE(realArrayAlloc, 2) /= 5) CALL zzrc( 40_4 )
    IF (SIZE(realArrayAlloc, 3) /= 5) CALL zzrc( 50_4 )


    CALL zzrc( assignRealArray( ) )


    CONTAINS

        INTEGER(KIND=4) FUNCTION assignRealArray( )

            REAL(8) :: checkValue
            logical(4), external :: precision_r8

            REAL(8), DIMENSION( 5,25,1 ) :: newSizeShape =&
                RESHAPE((/ ((i * 3.14_8), i = 125, 1, -1) /), (/ 5,25,1 /))

            assignRealArray = 0_4


            realArrayAlloc = newSizeShape

            IF (.NOT. ALLOCATED( realArrayAlloc )) THEN
                assignRealArray = 60_4

            ELSE IF (SIZE( realArrayAlloc ) /= 125) THEN
                assignRealArray = 70_4

            ELSE IF (SIZE(realArrayAlloc, 1) /=  5) THEN
                assignRealArray = 80_4

            ELSE IF (SIZE(realArrayAlloc, 2) /= 25) THEN
                assignRealArray = 90_4

            ELSE IF (SIZE(realArrayAlloc, 3) /=  1) THEN
                assignRealArray = 100_4
            END IF


            j = 1
            DO WHILE ((j < 26)  .AND. (assignRealArray == 0))
                i = 1

                DO WHILE ((i < 6)  .AND. (assignRealArray == 0))
                    checkValue = (((25 - j) * 5) + (5 - i + 1)) * 3.14_8
                    IF (KIND( realArrayAlloc( i,j,1 ) ) /= 16) THEN
                        PRINT *, "KIND(realArrayAlloc(", i, ",", j,&
                                   ", 1 ) =", KIND( realArrayAlloc( i,j,1 ) )
                        PRINT *, "Should be 8"

                        assignRealArray = 110_4

                    ELSE IF (.not. precision_r8(real(realArrayAlloc(i,j,1),8), checkValue)) THEN
                        PRINT *, "newSizeShape(", i, ",", j, " 1 ) =",&
                                    newSizeShape( i,j,1 )
                        PRINT *, "realArrayAlloc(", i, ",", j, " 1 ) =",&
                                    realArrayAlloc( i,j,1 )
                        PRINT *, "Should be", checkValue

                        assignRealArray = 120_4
                    END IF

                    i = i + 1
                END DO

                j = j + 1
            END DO

        END FUNCTION assignRealArray

END PROGRAM allocatedAllocatable04
