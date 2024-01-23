!*  ===================================================================
!*
!*                               CHARACTER Intrinsic Type
!*
!*  DATE                       : September 22, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is
!*                               a Deferred Length Unallocated/Allocated
!*                               ALLOCATABLE Array Dummy Argument of
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

PROGRAM deferredAssumed05

    CHARACTER(:), ALLOCATABLE :: deferred( : )
    CHARACTER(3) :: assumed( 17 ) =&
        (/ 'one', 'two', 'thr', 'ee ', 'fiv', 'six', 'sev', 'en ',&
           'eig', 'ht ', 'nin', 'ten', 'ele', 'ven', 'twe', 'lve', 'thi' /)


    IF ( ALLOCATED( deferred ) ) ERROR STOP 10_4

    CALL DeferredAssign(deferred, assumed, 20_4)


    IF (.NOT. ALLOCATED( deferred )) ERROR STOP 30_4

    CALL DeferredAssign(deferred,&
        (assumed( 1:8 ) // (/ (assumed( i )( 2: ), i = 17, 10, -1) /)), 40_4)


    IF (.NOT. ALLOCATED( deferred )) ERROR STOP 50_4

    CALL DeferredAssign(deferred, (/ (assumed( i ), i = 15, 3, -1) /), 60_4)


    CONTAINS


        SUBROUTINE DeferredAssign(deferred, assumed, failRC)
            CHARACTER(:), ALLOCATABLE :: deferred( : )
            CHARACTER(*) :: assumed( : )
            INTEGER(4) :: failRC


            deferred = assumed

            IF (.NOT. ALLOCATED( deferred )) CALL zzrc( failRC )

            DO i = 1, SIZE( deferred )
                PRINT *, i, "'", deferred( i ), "'"
            END DO

        END SUBROUTINE DeferredAssign

END PROGRAM deferredAssumed05
