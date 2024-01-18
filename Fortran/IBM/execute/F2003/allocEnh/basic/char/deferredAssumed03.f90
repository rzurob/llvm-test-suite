!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : deferredAssumed03 - Basic Tests:
!*                               CHARACTER Intrinsic Type
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : September 22, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is
!*                               an Assumed Length Allocated ALLOCATABLE
!*                               Array Dummy Argument of Type CHARACTER
!*  SECONDARY FUNCTIONS TESTED : expr is a Deferred Length Allocated
!*                               ALLOCATABLE of Type CHARACTER
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

        SUBROUTINE AssumedAssign(assumed, deferred, rc)
            CHARACTER(*), ALLOCATABLE :: assumed( : )
            CHARACTER(:), ALLOCATABLE :: deferred( : )
            INTEGER(4) :: rc


            IF (.NOT. ALLOCATED( assumed ))     CALL zzrc( (rc + 1_4) )
            IF (.NOT. ALLOCATED( deferred ))    CALL zzrc( (rc + 2_4) )

            assumed = deferred

            IF (.NOT. ALLOCATED( assumed ))     CALL zzrc( (rc + 3_4) )

        END SUBROUTINE AssumedAssign

END MODULE mModule

PROGRAM deferredAssumed03
    USE mModule

    INTEGER(4) :: i

    CHARACTER(13), ALLOCATABLE :: assumed( : )
    CHARACTER(:), ALLOCATABLE :: deferred( : )


    ALLOCATE(assumed( 5 ), SOURCE=(/ ('1a2b3c4d5e6f7', i = 1, 5) /))
    ALLOCATE(deferred( 3 ), SOURCE=(/ ('ghIJklMNopQRst', i = 1, 3) /))


    CALL AssumedAssign(assumed, deferred, 10_4)


    PRINT *, SIZE( assumed ), LEN( assumed )

    IF (SIZE( assumed ) /= 3)   CALL zzrc( 21_4 )
    IF (LEN( assumed ) /= 13)   CALL zzrc( 22_4 )


    DO i = 1, 3
        PRINT *, i, LEN_TRIM( assumed( i ) ), "'", assumed( i ), "'"

        IF (LEN_TRIM( assumed( i ) ) /= 13)     CALL zzrc( (30_4 + i) )
        IF (assumed( i ) /= 'ghIJklMNopQRs')    CALL zzrc( (40_4 + i) )
    END DO


    PRINT *
    DEALLOCATE( deferred )
    ALLOCATE(deferred( 9 ), SOURCE=(/ ('aZBycXDwEvF', i = 1, 9) /))


    CALL AssumedAssign(assumed, deferred, 50_4)


    PRINT *, SIZE( assumed ), LEN( assumed )

    IF (SIZE( assumed ) /= 9)   CALL zzrc( 61_4 )
    IF (LEN( assumed ) /= 13)   CALL zzrc( 62_4 )


    DO i = 1, 9
        PRINT *, i, LEN_TRIM( assumed( i ) ), "'", assumed( i ), "'"

        IF (LEN_TRIM( assumed( i ) ) /= 11)     CALL zzrc( (70_4 + i) )
        IF (assumed( i ) /= 'aZBycXDwEvF  ')    CALL zzrc( (80_4 + i) )
    END DO

END PROGRAM deferredAssumed03
