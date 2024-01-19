!*  ===================================================================
!*
!*                               expr Contain References to variable
!*
!*  DATE                       : October 16, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is
!*                               an Allocated ALLOCATABLE Array of INTEGER
!*                               Intrinsic Type
!*  SECONDARY FUNCTIONS TESTED : expr is an Array Section of variable
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 7
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

PROGRAM exprIsArraySection01

    INTEGER(8), ALLOCATABLE :: intArrAlloc( :,: )


    ALLOCATE(intArrAlloc( 10,10 ), SOURCE=RESHAPE((/&
            ((INT(((i * 10) + j), 8), j = 0, 9), i = 0, 9) /), (/ 10,10 /)))
    CALL CheckAndDump(10, 10, 'Initial Allocation (10x10)', 10_4)


    intArrAlloc = intArrAlloc( 3:8,3:8 )
    CALL CheckAndDump(6, 6, 'Sub-Section in both Dimensions (6x6)', 20_4)

    intArrAlloc = intArrAlloc( 2:5,: )
    CALL CheckAndDump(4, 6, 'Sub-Section in 1st Dimension (4x6)', 30_4)

    intArrAlloc = intArrAlloc( :,2:5 )
    CALL CheckAndDump(4, 4, 'Sub-Section in 2nd Dimension (4x4)', 40_4)


    intArrAlloc = intArrAlloc( 3:1:-1,3:1:-1 )
    CALL CheckAndDump(3, 3, 'Sub-Section Reverse both Dimensions (3x3)', 50_4)

    intArrAlloc = intArrAlloc( 3:2:-1,: )
    CALL CheckAndDump(2, 3, 'Sub-Section Reverse in 1st Dimension (2x3)', 60_4)

    intArrAlloc = intArrAlloc( :,3:2:-1 )
    CALL CheckAndDump(2, 2, 'Sub-Section Reverse in 2nd Dimension (2x2)', 70_4)


    intArrAlloc = intArrAlloc( :,: )
    CALL CheckAndDump(2, 2, 'Array Section (2x2)', 80_4)


    CONTAINS

        SUBROUTINE CheckAndDump(jSize, iSize, title, failRC)
            INTEGER :: jSize
            INTEGER :: iSize
            CHARACTER(*) :: title
            INTEGER(4) :: failRC


            CHARACTER(8) :: fmt


            IF (.NOT. ALLOCATED( intArrAlloc )) CALL zzrc( failRC )


            PRINT *
            PRINT *, title, ':'
            PRINT *, 'SIZE(intArrAlloc, 1) =',&
                        SIZE(intArrAlloc, 1), '(', jSize, ')'
            PRINT *, 'SIZE(intArrAlloc, 2) =',&
                        SIZE(intArrAlloc, 2), '(', iSize, ')'


            WRITE(fmt, '("(",I2,"(I3))")') SIZE(intArrAlloc, 2)

            DO j = 1, SIZE(intArrAlloc, 1)
                PRINT fmt, intArrAlloc( j,: )
            END DO


            IF (SIZE(intArrAlloc, 1) /= jSize) CALL zzrc( (failRC + 1_4) )
            IF (SIZE(intArrAlloc, 2) /= iSize) CALL zzrc( (failRC + 2_4) )

        END SUBROUTINE CheckAndDump

END PROGRAM exprIsArraySection01
