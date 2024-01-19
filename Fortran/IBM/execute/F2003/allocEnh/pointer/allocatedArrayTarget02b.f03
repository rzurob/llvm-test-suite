!*  ===================================================================
!*
!*  DATE                       : October 13, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is a
!*                               POINTER to an Allocated ALLOCATABLE Array of
!*                               Intrinsic CHARACTER Type of Deferred Length,
!*                               and
!*  SECONDARY FUNCTIONS TESTED : the value of expr has a different Shape
!*                               Result
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
!*  If variable is a pointer, the value of expr is assigned to the target
!*  of variable.
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

PROGRAM allocatedArrayTarget02b

    CHARACTER(:), POINTER :: defPtr( : )
    CHARACTER(:), TARGET, ALLOCATABLE :: defArrAlloc( : )


    ALLOCATE(CHARACTER(5) :: defArrAlloc( 10 ))

    defArrAlloc = (/ 'abcde', 'FGHIJ', 'klmno', 'PQRST', 'uvwxy',&
                     'ABCDE', 'fghij', 'KLMNO', 'pqrst', 'UVWXY' /)


    defPtr => defArrAlloc
    CALL CheckDef(10, 5, 10_4)

    defPtr(:5) = (/ ('12345', i = 1, 5) /)
    CALL CheckDef(10, 5, 20_4)

    defPtr = (/ ('zZzZz', i = 1, 10) /)
    CALL CheckDef(10, 5, 30_4)

    defPtr = (/ ('9a8b7', i = 1, 10) /)
    CALL CheckDef(10, 5, 40_4)

    defPtr = (/ ('test', i = 1, 10) /)
    CALL CheckDef(10, 5, 50_4)

    defPtr = (/ ('retest', i = 1, 10) /)
    CALL CheckDef(10, 5, 60_4)


    CONTAINS


        SUBROUTINE CheckDef(iMax, length, failRC)
            INTEGER :: iMax
            INTEGER :: length
            INTEGER(4) :: failRC

            CHARACTER(34) :: fmt


            IF (.NOT. ALLOCATED( defArrAlloc )) CALL zzrc( failRC )


            WRITE(fmt, 10) SIZE(defArrAlloc, 1), LEN( defArrAlloc )
10          FORMAT("(I1,')  ',", I2, "('(',A", I1, ",')'))" )

            PRINT fmt, (failRC / 10_4), defArrAlloc


            IF (SIZE(defArrAlloc, 1) /= iMax) CALL zzrc( (failRC + 1_4) )
            IF (LEN( defArrAlloc ) /= length) CALL zzrc( (failRC + 2_4) )

        END SUBROUTINE CheckDef

END PROGRAM allocatedArrayTarget02b
