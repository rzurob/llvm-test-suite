!*  ===================================================================
!*
!*  DATE                       : October 25, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is a an
!*                               Allocated ALLOCATABLE Scalar of CHARACTER
!*                               with Deferred Length
!*  SECONDARY FUNCTIONS TESTED : and expr uses a POINTER that has a TARGET of
!*                               variable and the value of expr has a different
!*                               Length Type Parameter Result
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

PROGRAM ptrToVarScalarAsExpr02

    INTEGER :: length = 1
    INTEGER :: lastDigit = 0
    INTEGER(4) :: rc = 5_4

    CHARACTER(:), POINTER :: charScalarPtr
    CHARACTER(:), TARGET, ALLOCATABLE :: charScalarAlloc


    ALLOCATE(CHARACTER(1) :: charScalarAlloc)
    charScalarAlloc = '0'

    CALL CheckIt(length, lastDigit, rc)


    DO i = 1, 63
        charScalarPtr => charScalarAlloc

        IF (charScalarPtr( LEN( charScalarPtr ): ) == '0') THEN
            lastDigit = 1
            charScalarAlloc( LEN( charScalarAlloc ): ) = '1'
        ELSE
            lastDigit = 0
            charScalarAlloc = charScalarPtr // '0'

            length = length + 1
        END IF

        rc = rc + 5_4
        CALL CheckIt(length, lastDigit, rc)
    END DO


    PRINT *

    lastDigit = 0
    charScalarAlloc( LEN( charScalarAlloc ): ) = '0'

    rc = rc + 5_4
    CALL CheckIt(length, lastDigit, rc)


    DO i = 31, 1, -1
        charScalarPtr => charScalarAlloc
        charScalarAlloc = charScalarPtr( 2: )

        rc = rc + 5_4
        CALL CheckIt(i, lastDigit, rc)
    END DO


    CONTAINS

        SUBROUTINE CheckIt(length, digit, failRC)
            INTEGER :: length
            INTEGER :: digit
            INTEGER(4) :: failRC

            INTEGER :: value
            CHARACTER(5) :: fmt


            IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( failRC )


            WRITE(fmt, '("(B",I2,")")') LEN( charScalarAlloc )
            READ(charScalarAlloc, fmt) value

            PRINT *, LEN( charScalarAlloc ), '(', length, ')',&
                        digit, value, '"', charScalarAlloc, '"'

            IF (LEN( charScalarAlloc ) /= length) CALL zzrc( (failRC + 1_4) )
            IF (value /= (2**LEN( charScalarAlloc ) - 2 + digit))&
                                            CALL zzrc( (failRC + 2_4) )

        END SUBROUTINE CheckIt

END PROGRAM ptrToVarScalarAsExpr02
