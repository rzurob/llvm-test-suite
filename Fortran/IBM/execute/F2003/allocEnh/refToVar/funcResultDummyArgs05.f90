!*  ===================================================================
!*
!*                               expr Contain References to variable
!*
!*  DATE                       : October 20, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of CHARACTER
!*                               and expr calls a User Defined FUNCTION
!*                               that has variable as the Actual Argument.
!*  SECONDARY FUNCTIONS TESTED : Elements of the corresponding Dummy Argument
!*                               are Assigned to the FUNCTION's Result.
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

PROGRAM funcResultDummyArgs05

    CHARACTER(3) :: charArr( 10 ) = (/ 'abc', 'DEF', 'ghi', 'JKL', 'mno',&
                                       'PQR', 'stu', 'VWX', 'YZA', 'BCd' /)

    CHARACTER(3), ALLOCATABLE :: charArrAlloc( : )


    ALLOCATE(charArrAlloc( 20 ), SOURCE=(/ charArr,charArr( 10:1:-1 ) /))
    CALL Dump( 10_4 )


    charArrAlloc = ArrayAssign1(charArrAlloc, 0)
    CALL Dump( 20_4 )

    charArrAlloc = ArrayAssign2( charArrAlloc )
    CALL Dump( 30_4 )

    DO i = 1_4, 8_4
        charArrAlloc = ArrayAssign1(charArrAlloc, 1)
        CALL Dump( (40_4 + i) )
    END DO


    CONTAINS


        SUBROUTINE Dump( failRC )
            INTEGER(4) :: failRC

            CHARACTER(47) :: fmt


            IF (.NOT. ALLOCATED( charArrAlloc )) CALL zzrc( failRC )


            WRITE(fmt, 10) SIZE( charArrAlloc )
10          FORMAT('("S() = ",I2,", L() =",I2,", (",',I2,'(A3,","),")")')


            PRINT fmt, SIZE( charArrAlloc ), LEN( charArrAlloc ), charArrAlloc
!  fmt  ==   ("S() = 20, L() =n3, (",20(I3,","),")")

        END SUBROUTINE Dump


        FUNCTION ArrayAssign1(arg, test)
            CHARACTER(3) :: arg( : )
            INTEGER :: test

            CHARACTER(3), ALLOCATABLE :: ArrayAssign1( : )


            IF (test == 0) THEN
                ArrayAssign1 = arg( 2:(SIZE( arg ) - 1) )

            ELSE IF (test == 1) THEN
                ArrayAssign1 = ArrayAssign2( arg )
                ArrayAssign1 = ArrayAssign1( 2:(SIZE( ArrayAssign1 ) - 1) )
            END IF

        END FUNCTION ArrayAssign1


        FUNCTION ArrayAssign2( arg )
            CHARACTER(3) :: arg( : )

            CHARACTER(3), ALLOCATABLE :: ArrayAssign2( : )


            ArrayAssign2 = arg( : )( 3: ) //&
                           arg( SIZE( arg ):1:-1 )( :1 ) // arg( : )( 2: )

        END FUNCTION ArrayAssign2

END PROGRAM funcResultDummyArgs05
