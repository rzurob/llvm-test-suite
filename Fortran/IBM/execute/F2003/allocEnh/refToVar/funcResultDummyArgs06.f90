!*  ===================================================================
!*
!*                               expr Contain References to variable
!*
!*  DATE                       : October 20, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of CHARACTER
!*                               (with Deferred Length) and expr calls a
!*                               User Defined FUNCTION that has variable
!*                               as the Actual Argument.
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

MODULE mModule

    CHARACTER(:), ALLOCATABLE :: charArrAlloc( :,: )


    CONTAINS


        SUBROUTINE Dump(length, size1, size2, failRC)
            INTEGER :: length
            INTEGER :: size1
            INTEGER :: size2
            INTEGER(4) :: failRC

            CHARACTER(12) :: fmt


            IF (.NOT. ALLOCATED( charArrAlloc )) CALL zzrc( failRC )


            PRINT *
            PRINT *, 'Expect: ', length, size1, size2
            PRINT *, 'Found:  ', LEN( charArrAlloc ),&
                     SIZE(charArrAlloc, 1), SIZE(charArrAlloc, 2)

            WRITE(fmt, 10) SIZE(charArrAlloc, 2), LEN( charArrAlloc )
10          FORMAT('(',I1,'(A',I2,'," "))')

            DO i = 1, SIZE(charArrAlloc, 1)
                PRINT fmt, charArrAlloc( i,: )
            END DO


            IF (LEN( charArrAlloc ) /= length) CALL zzrc( (failRC + 1_4) )

            IF (SIZE(charArrAlloc, 1) /= size1) CALL zzrc( (failRC + 2_4) )
            IF (SIZE(charArrAlloc, 2) /= size2) CALL zzrc( (failRC + 3_4) )

        END SUBROUTINE Dump


        FUNCTION AssignIt(arg, test)
            CHARACTER(*) :: arg( :,: )
            INTEGER :: test

            CHARACTER(:), ALLOCATABLE :: AssignIt( :,: )


            IF (test == 0) THEN
                AssignIt =&
                    arg( :,: )( :2 ) // arg( :,: )( 3: ) // arg( :,: )( 3: )

            ELSE IF (test == 1) THEN
                AssignIt = RESHAPE(&
                    (/ arg( :3,:2 ), arg( :3,3: ), arg( 2:,:2 ),&
                       arg( 2:,3: ), arg( :3,3: ), arg( :3,:2 ) /), (/ 6,6 /))

            ELSE IF (test == 2) THEN
                AssignIt = arg( 2:5,5:2:-1 )( 2:5 )
            END IF

        END FUNCTION AssignIt

END MODULE mModule

PROGRAM funcResultDummyArgs06
    USE mModule

    CHARACTER(4) :: charArr( 4,4 ) =&
        RESHAPE((/ 'abcd', 'efgh', 'ijkl', 'mnop',&
                   'QRST', 'UVWX', 'YZAB', 'CDEF',&
                   'ghij', 'klmn', 'opqr', 'stuv',&
                   'WXYZ', 'ABCD', 'EFGH', 'IJKL' /), (/ 4,4 /))


    charArrAlloc = charArr
    CALL Dump(4, 4, 4, 10_4)


    charArrAlloc = AssignIt(charArrAlloc, 0)
    CALL Dump(6, 4, 4, 20_4)

    charArrAlloc = AssignIt(charArrAlloc, 1)
    CALL Dump(6, 6, 6, 30_4)

    charArrAlloc = AssignIt(charArrAlloc, 2)
    CALL Dump(4, 4, 4, 40_4)

END PROGRAM funcResultDummyArgs06
