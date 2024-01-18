!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : assumedDummyArg02 - Basic Tests:
!*                               CHARACTER Intrinsic Type
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : September 22, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Assumed Length ALLOCATABLE Array Dummy
!*                               Argument of Type CHARACTER
!*  SECONDARY FUNCTIONS TESTED : The corresponding Actual Argument is an
!*                               Allocated/Unallocated ALLOCATABLE
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

MODULE mModule

    CONTAINS

        FUNCTION CharFn(chr, n, length)
            CHARACTER(*) :: chr
            INTEGER :: n
            INTEGER :: length

            CHARACTER( length ), ALLOCATABLE :: CharFn

            ALLOCATE(CharFn, SOURCE=REPEAT(chr, n))
            PRINT *, "CharFn() '", chr, "',", length,&
                        n, LEN( chr ), (n * LEN( chr )), "'", CharFn, "'"

        END FUNCTION CharFn

END MODULE mModule

PROGRAM assumedDummyArg02

    INTERFACE
        SUBROUTINE AssumedAssign(charArrayArg, chr, n)
            USE mModule
            CHARACTER(*), ALLOCATABLE :: charArrayArg( : )
            CHARACTER(*) :: chr
            INTEGER :: n
        END SUBROUTINE AssumedAssign
    END INTERFACE


    INTEGER(4) :: i

    CHARACTER(45) :: charCheck
    CHARACTER(45), ALLOCATABLE :: charArrayVar( : )


    IF ( ALLOCATED( charArrayVar ) )    CALL zzrc( 10_4 )

    CALL AssumedAssign(charArrayVar, 'zYxWuVtSr', 5)

    IF (.NOT. ALLOCATED( charArrayVar )) CALL zzrc( 11_4 )
    IF (SIZE( charArrayVar ) /= 3)       CALL zzrc( 12_4 )

    DO i = 1, 3
        length = MIN(((3 + i) * 9), 45)
        PRINT *, i, length, LEN_TRIM( charArrayVar( i ) ),&
                 LEN( charArrayVar( i ) ), "'", charArrayVar( i ), "'"

        IF (LEN( charArrayVar( i ) ) /= 45)             CALL zzrc( (15_4 + i) )
        IF (LEN_TRIM( charArrayVar( i ) ) /= length)    CALL zzrc( (20_4 + i) )

        charCheck = REPEAT('zYxWuVtSr', (4 + i))
        IF (charArrayVar( i ) /= charCheck( 1:length )) CALL zzrc( (25_4 + i) )
    END DO

END PROGRAM assumedDummyArg02

SUBROUTINE AssumedAssign(charArrayArg, chr, n)
    USE mModule

    CHARACTER(*), ALLOCATABLE :: charArrayArg( : )
    CHARACTER(*) :: chr
    INTEGER :: n

    charArrayArg =&
        (/ (CharFn(chr, (n + i), LEN( charArrayArg )), i = -1, 1) /)

END SUBROUTINE AssumedAssign
