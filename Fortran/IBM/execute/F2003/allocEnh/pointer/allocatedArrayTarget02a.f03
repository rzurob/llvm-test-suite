!*  ===================================================================
!*
!*  DATE                       : October 13, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is a
!*                               POINTER to an Allocated ALLOCATABLE Array
!*                               of Intrinsic CHARACTER Type, and
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

PROGRAM allocatedArrayTarget02a

    CHARACTER(5), POINTER :: chrPtr( : )
    CHARACTER(5), TARGET, ALLOCATABLE :: chrArrAlloc( : )


    ALLOCATE(chrArrAlloc( 10 ),&
        SOURCE=(/ 'abcde', 'FGHIJ', 'klmno', 'PQRST', 'uvwxy',&
                  'ABCDE', 'fghij', 'KLMNO', 'pqrst', 'UVWXY' /))


    chrPtr => chrArrAlloc
    CALL CheckChr(10, 5, 10_4)

    chrPtr(1:5) = (/ ('12345', i = 1, 5) /)
    CALL CheckChr(10, 5, 20_4)

    chrPtr = (/ ('zZzZz', i = 1, 10) /)
    CALL CheckChr(10, 5, 30_4)

    chrPtr = (/ ('9a8b7', i = 1, 10) /)
    CALL CheckChr(10, 5, 40_4)

    chrPtr = (/ ('test', i = 1, 10) /)
    CALL CheckChr(10, 5, 50_4)

    chrPtr = (/ ('retest', i = 1, 10) /)
    CALL CheckChr(10, 5, 60_4)


    CONTAINS


        SUBROUTINE CheckChr(iMax, length, failRC)
            INTEGER :: iMax
            INTEGER :: length
            INTEGER(4) :: failRC

            CHARACTER(34) :: fmt


            IF (.NOT. ALLOCATED( chrArrAlloc )) CALL zzrc( failRC )


            WRITE(fmt, 10) SIZE(chrArrAlloc, 1), LEN( chrArrAlloc )
10          FORMAT("(I1,')  ',", I2, "('(',A", I1, ",')'))" )

            PRINT fmt, (failRC / 10_4), chrArrAlloc


            IF (SIZE(chrArrAlloc, 1) /= iMax) CALL zzrc( (failRC + 1_4) )
            IF (LEN( chrArrAlloc ) /= length) CALL zzrc( (failRC + 2_4) )

        END SUBROUTINE CheckChr

END PROGRAM allocatedArrayTarget02a
