!*  ===================================================================
!*
!*                               CHARACTER Intrinsic Type
!*
!*  DATE                       : September 22, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is
!*                               an Assumed Length Unallocated ALLOCATABLE
!*                               Array Dummy Argument of Type CHARACTER
!*  SECONDARY FUNCTIONS TESTED : expr is a Deferred Length Allocated
!*                               ALLOCATABLE of Type CHARACTER
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
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mModule

    CHARACTER(7), ALLOCATABLE :: assumed( : )
    CHARACTER(:), ALLOCATABLE :: deferred( : )

    CONTAINS

        CHARACTER(13) FUNCTION ArrayInit(str, n)
            CHARACTER(*) :: str
            INTEGER :: n

            ArrayInit = str( n:(n + 12) )

        END FUNCTION ArrayInit

END MODULE mModule


PROGRAM deferredAssumed02
    USE mModule

    INTEGER(4) :: i

    CHARACTER(26) :: alphabet = 'aBcDeFgHiJkLmNoPqRsTuVwXyZ'


    IF ( ALLOCATED( deferred ) )        CALL zzrc( 10_4 )

    ALLOCATE(deferred( 7 ), SOURCE=(/ (ArrayInit(alphabet, i), i = 1, 7) /))

    IF (.NOT. ALLOCATED( deferred ))    CALL zzrc( 11_4 )
    IF ( ALLOCATED( assumed ) )         CALL zzrc( 12_4 )


    CALL AssumedAssign(assumed, 20_4)


    IF (.NOT. ALLOCATED( assumed )) CALL zzrc( 30_4 )

    PRINT *, SIZE( assumed ), LEN( assumed )

    IF (SIZE( assumed ) /= 7)       CALL zzrc( 31_4 )
    IF (LEN( assumed ) /= 7)        CALL zzrc( 32_4 )


    DO i = 1, 7
        PRINT *, i, LEN_TRIM( assumed( i ) ),&
                '"', assumed( i ), '" "', deferred( i ), '"'

        IF (LEN_TRIM( assumed( i ) ) /= 7)          CALL zzrc( (40_4 + i) )
        IF (assumed( i ) /= alphabet( i:(i + 6) ))  CALL zzrc( (50_4 + i) )
    END DO


    CONTAINS

        SUBROUTINE AssumedAssign(assumed, rcBase)
            CHARACTER(*), ALLOCATABLE :: assumed( : )
            INTEGER(4) :: rcBase


            IF ( ALLOCATED( assumed ) )     CALL zzrc( (rcBase + 1) )

            assumed = deferred

            IF (.NOT. ALLOCATED( assumed )) CALL zzrc( (rcBase + 2) )

        END SUBROUTINE AssumedAssign

END PROGRAM deferredAssumed02
