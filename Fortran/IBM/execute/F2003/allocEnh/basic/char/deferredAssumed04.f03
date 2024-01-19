!*  ===================================================================
!*
!*                               CHARACTER Intrinsic Type
!*
!*  DATE                       : September 22, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is
!*                               a Deferred Length Unallocated/Allocated
!*                               ALLOCATABLE Scalar Dummy Argument of
!*                               Type CHARACTER
!*  SECONDARY FUNCTIONS TESTED : expr is an Assumed Length Allocated
!*                               ALLOCATABLE of Type CHARACTER
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
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mModule
    CHARACTER(:), ALLOCATABLE :: deferred
END MODULE mModule


PROGRAM deferredAssumed04

    INTERFACE
        SUBROUTINE DeferredAssign(assumed, rcBase)
            USE mModule
            CHARACTER(*) :: assumed
            INTEGER(4) :: rcBase
        END SUBROUTINE DeferredAssign

        SUBROUTINE Expected(length, assumed, rcBase)
            USE mModule
            INTEGER :: length
            CHARACTER(length) :: assumed
            INTEGER(4) :: rcBase
        END SUBROUTINE Expected
    END INTERFACE

    CHARACTER(3) :: char3Var = 'zyx'
    CHARACTER(5) :: char5Var = 'ABCDE'


    CALL DeferredAssign(char5Var, 10_4)
    CALL Expected(LEN( char5Var ), char5Var, 20_4)


    CALL DeferredAssign(char3Var, 30_4)
    CALL Expected(LEN( char3Var ), char3Var, 40_4)


    CALL DeferredAssign(char5Var, 50_4)
    CALL Expected(LEN( char5Var ), char5Var, 60_4)

END PROGRAM deferredAssumed04


SUBROUTINE DeferredAssign(assumed, rcBase)
    USE mModule

    CHARACTER(*) :: assumed
    INTEGER(4) :: rcBase


    IF (rcBase == 10_4) THEN
        IF ( ALLOCATED( deferred ) )    CALL zzrc( (rcBase + 1_4) )

    ELSE
        IF (.NOT. ALLOCATED( deferred ))CALL zzrc( (rcBase + 2_4) )
    END IF


    deferred = assumed


    IF (.NOT. ALLOCATED( deferred ))    CALL zzrc( (rcBase + 3_4) )

END SUBROUTINE DeferredAssign


SUBROUTINE Expected(length, assumed, rcBase)
    USE mModule

    INTEGER :: length
    CHARACTER(length) :: assumed
    INTEGER(4) :: rcBase


    IF (.NOT. ALLOCATED( deferred ))    CALL zzrc( (rcBase + 1_4) )

    PRINT *, length, LEN( deferred ), LEN_TRIM( deferred ),&
                            "'", deferred, "' (", assumed, ")"

    IF (LEN( deferred ) /= length)      CALL zzrc( (rcBase + 2_4) )
    IF (LEN_TRIM( deferred ) /= length) CALL zzrc( (rcBase + 3_4) )
    IF (deferred /= assumed)            CALL zzrc( (rcBase + 4_4) )

END SUBROUTINE Expected
