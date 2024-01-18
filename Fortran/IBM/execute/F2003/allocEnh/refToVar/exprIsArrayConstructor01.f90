!*  ===================================================================
!*
!*                               expr Contain References to variable
!*
!*  DATE                       : October 16, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is
!*                               an Allocated ALLOCATABLE Array of INTEGER
!*                               Intrinsic Type
!*  SECONDARY FUNCTIONS TESTED : expr is an Array Constructor that references
!*                               Elements of variable
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
!*  Both variable and expr may contain references to any portion of variable.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM exprIsArrayConstructor01

    INTEGER(4) :: failRC = 10_4
    INTEGER :: expectedSize = 100

    INTEGER, ALLOCATABLE :: intArrAlloc( : )


    ALLOCATE(intArrAlloc( 100 ), SOURCE=(/ (i, i = 0, 99) /))

    CALL Dump(expectedSize, failRC)


    DO WHILE (SIZE( intArrAlloc ) > 1)
        intArrAlloc = (/ (intArrAlloc( i ), i = 2, SIZE( intArrAlloc ), 2) /)

        failRC = failRC + 10_4
        expectedSize = expectedSize / 2

        CALL Dump(expectedSize, failRC)
    END DO


    CONTAINS

        SUBROUTINE Dump(expectedSize, failRC)
            INTEGER :: expectedSize
            INTEGER(4) :: failRC

            CHARACTER(8) :: fmt


            IF (.NOT. ALLOCATED( intArrAlloc )) CALL zzrc( failRC )


            IF (MOD(SIZE( intArrAlloc ), 10) == 0) THEN
                length = 10

            ELSE IF (MOD(SIZE( intArrAlloc ), 5) == 0) THEN
                length = 5

            ELSE IF (MOD(SIZE( intArrAlloc ), 3) == 0) THEN
                length = 3

            ELSE
                length = 1
            END IF


            WRITE(fmt, 10) length
10          FORMAT('(',I2,'(I3))')


            PRINT *
            PRINT *, expectedSize
            DO i = 1, SIZE( intArrAlloc ), length
                PRINT fmt, intArrAlloc( i:(i + length - 1) )
            END DO


            IF (SIZE( intArrAlloc ) /= expectedSize)&
                                CALL zzrc( (failRc + 1_4) )

        END SUBROUTINE Dump

END PROGRAM exprIsArrayConstructor01
