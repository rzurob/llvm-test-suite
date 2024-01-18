!*  ===================================================================
!*
!*                               CHARACTER Intrinsic Type
!*
!*  DATE                       : September 22, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is
!*                               an Assumed Length Unallocated/Allocated
!*                               ALLOCATABLE Scalar Dummy Argument of
!*                               Type CHARACTER
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

PROGRAM deferredAssumed01

    INTERFACE
        SUBROUTINE AssignToAssumed(assumed, deferred, failRCBase)
            CHARACTER(*), ALLOCATABLE :: assumed
            CHARACTER(:), ALLOCATABLE :: deferred
            INTEGER(4) :: failRCBase
        END SUBROUTINE AssignToAssumed
    END INTERFACE

    CHARACTER(5), ALLOCATABLE :: assumed
    CHARACTER(:), ALLOCATABLE :: deferred


    IF ( ALLOCATED( deferred ) )        CALL zzrc( 10_4 )

    ALLOCATE(deferred, SOURCE='abc')

    IF ( ALLOCATED( assumed ) )         CALL zzrc( 11_4 )
    IF (.NOT. ALLOCATED( deferred ))    CALL zzrc( 12_4 )

    CALL AssignToAssumed(assumed, deferred, 20_4)

    IF (.NOT. ALLOCATED( assumed )) CALL zzrc( 30_4 )

    PRINT *, LEN( assumed ), LEN_TRIM( assumed ), "'", assumed, "'"

    IF (LEN( assumed ) /= 5)        CALL zzrc( 31_4 )
    IF (LEN_TRIM( assumed ) /= 3)   CALL zzrc( 32_4 )
    IF (assumed /= 'abc')           CALL zzrc( 32_4 )


    DEALLOCATE( deferred )


    IF ( ALLOCATED( deferred ) )        CALL zzrc( 40_4 )

    ALLOCATE(deferred, SOURCE='ZYXWVUT')

    IF (.NOT. ALLOCATED( assumed ))     CALL zzrc( 41_4 )
    IF (.NOT. ALLOCATED( deferred ))    CALL zzrc( 42_4 )

    CALL AssignToAssumed(assumed, deferred, 50_4)

    IF (.NOT. ALLOCATED( assumed )) CALL zzrc( 60_4 )

    PRINT *, LEN( assumed ), LEN_TRIM( assumed ), "'", assumed, "'"

    IF (LEN( assumed ) /= 5)        CALL zzrc( 61_4 )
    IF (LEN_TRIM( assumed ) /= 5)   CALL zzrc( 62_4 )
    IF (assumed /= 'ZYXWV')         CALL zzrc( 62_4 )

END PROGRAM deferredAssumed01


SUBROUTINE AssignToAssumed(assumed, deferred, failRCBase)
    CHARACTER(*), ALLOCATABLE :: assumed
    CHARACTER(:), ALLOCATABLE :: deferred
    INTEGER(4) :: failRCBase


    IF (failRCBase == 20_4) THEN
        IF ( ALLOCATED( assumed ) )     CALL zzrc( (failRCBase + 1_4) )

    ELSE
        IF (.NOT. ALLOCATED( assumed )) CALL zzrc( (failRCBase + 2_4) )
    END IF


    assumed = deferred


    IF (.NOT. ALLOCATED( assumed ))     CALL zzrc( (failRCBase + 3_4) )

END SUBROUTINE AssignToAssumed
