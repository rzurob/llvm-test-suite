!*  ===================================================================
!*
!*                               expr Contain References to variable
!*
!*  DATE                       : October 17, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Intrinsic Type
!*  SECONDARY FUNCTIONS TESTED : and expr uses an Intrinsic FUNCTION where
!*                               variable is used as an Argument to the
!*                               FUNCTION
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
!*  Both variable and expr may contain references to any portion of variable.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM intrinsicFunction01

    INTEGER :: intArr( 10,10,10,10,10,10 )
    INTEGER, ALLOCATABLE :: intArrAlloc( :,:,:,:,:,: )

    intArr = RESHAPE((/ ((((((MOD(((i * 10**5) + (j * 10**4) + (k * 10**3)&
                                 + (l * 10**2) + (m * 10**1) + n), 127),&
          n = 0, 9), m = 0, 9), l = 0, 9), k =  0, 9), j = 0, 9), i = 0, 9)&
                                                /), (/ 10,10,10,10,10,10 /))


    intArrAlloc = intArr
    CALL Check((/ 10,10,10,10,10,10 /), intArr, 10_4)


    intArrAlloc = MOD(intArrAlloc( :,:,:,:,:,3:8 ), 127)
    CALL Check((/ 10,10,10,10,10,6 /), intArr( :,:,:,:,:,3:8 ), 20_4)


    intArrAlloc = MODULO(intArrAlloc( 3:8,:,:,:,:,: ), 127)
    CALL Check((/ 6,10,10,10,10,6 /), intArr( 3:8,:,:,:,:,3:8 ), 30_4)


    intArrAlloc = IACHAR( CHAR( intArrAlloc ) )
    CALL Check((/ 6,10,10,10,10,6 /), intArr( 3:8,:,:,:,:,3:8 ), 40_4)


    CONTAINS


        SUBROUTINE Check(sizeList, iA, failRC)
            INTEGER :: sizeList( : )
            INTEGER :: iA( :,:,:,:,:,: )
            INTEGER(4) :: failRC

            INTEGER(4) :: i


            IF (.NOT. ALLOCATED( intArrAlloc )) CALL zzrc( failRC )

            PRINT *
            DO i = 1_4, 6_4
                PRINT *, SIZE(intArrAlloc, i)
                IF (SIZE(intArrAlloc, i) /= sizeList( i ))&
                                            CALL zzrc( (failRC + i) )
            END DO

            IF ( ANY( (intArrAlloc /= iA) ) ) CALL zzrc( (failRC + 7_4) )

        END SUBROUTINE Check

END PROGRAM intrinsicFunction01
