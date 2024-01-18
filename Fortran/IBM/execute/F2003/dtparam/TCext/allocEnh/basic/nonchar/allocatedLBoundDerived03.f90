! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/allocEnh/basic/nonchar/allocatedLBoundDerived03.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*                               Non-CHARACTER Derived Type
!*
!*  DATE                       : August  9, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Derived Type
!*  SECONDARY FUNCTIONS TESTED : and LBOUND(expr) returns a value other than
!*                               "1" as the Lower Bound for an N-Dimension
!*                               Array (1<= N <= 7) -- N == 4
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : TYPE, ALLOCATABLE Attribute, Intrinsic
!*                               Assignment
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

MODULE base

    TYPE :: tBase(N1,K1)    ! (20,4)
        INTEGER, KIND        :: K1
        INTEGER, LEN         :: N1
        INTEGER(K1), POINTER :: ptBase

        CONTAINS
!            FINAL :: tBaseFinal
            procedure :: FINAL1 => tBaseFinal

    END TYPE tBase

    CONTAINS

        ELEMENTAL SUBROUTINE tBaseFinal( o )
!            TYPE(tBase(*,4)), INTENT(inout) :: o
            class(tBase(*,4)), INTENT(inout) :: o

            o%ptBase = -88

        END SUBROUTINE tBaseFinal

END MODULE base

MODULE mderived
    USE base

     TYPE, EXTENDS(tBase) :: derived    ! (20,4)
        INTEGER(K1), POINTER :: ptDerived

        CONTAINS
!            FINAL :: DerivedFinal
            procedure :: FINAL1 => DerivedFinal

     END TYPE derived

     CONTAINS

        ELEMENTAL SUBROUTINE DerivedFinal( o )
!            TYPE(derived(*,4)), INTENT(inout) :: o
            class(derived(*,4)), INTENT(inout) :: o

            call o%tbase%final1
            o%ptDerived = -99

        END SUBROUTINE DerivedFinal

END MODULE mderived

PROGRAM allocatedLBoundDerived03
    USE mderived

    INTEGER, TARGET :: idxBase( 2,1,1,2 ) =&
            RESHAPE((/ (i, i = 1, 4) /), (/ 2,1,1,2 /))

    INTEGER, TARGET :: idxDerived( 2,1,1,2 ) =&
            RESHAPE((/ (i, i = 1, 4) /), (/ 2,1,1,2 /))

    TYPE(derived(20,4)) :: derived( 0:0,-1:0,0:1,-1:-1 )

    TYPE(derived(:,4)), ALLOCATABLE :: derivedAlloc( :,:,:,: )


    ALLOCATE( derived(20,4) :: derivedAlloc( -1:0,-1:-1,0:0,0:1 ) )

    IF (.NOT. ALLOCATED( derivedAlloc )) CALL zzrc( 10_4 )

    IF (SIZE( derivedAlloc ) /= 4) CALL zzrc( 20_4 )

    IF (SIZE(derivedAlloc, 1) /= 2) CALL zzrc( 21_4 )
    IF (SIZE(derivedAlloc, 2) /= 1) CALL zzrc( 22_4 )
    IF (SIZE(derivedAlloc, 3) /= 1) CALL zzrc( 23_4 )
    IF (SIZE(derivedAlloc, 4) /= 2) CALL zzrc( 24_4 )


    DO i = 0, 1
        DO j = -1, 0
            derived( 0,j,i,-1 )%ptBase => NULL( )
            derived( 0,j,i,-1 )%ptDerived => NULL( )

            derivedAlloc( j,-1,0,i )%ptBase =>&
                                    idxBase( (j + 2),1,1,(i + 1))
            derivedAlloc( j,-1,0,i )%ptDerived =>&
                                    idxDerived( (j + 2),1,1,(i + 1))
        END DO
    END DO


    call derivedAlloc%final1
    derivedAlloc = derived


    IF (.NOT. ALLOCATED( derivedAlloc )) CALL zzrc( 30_4 )

    IF (SIZE( derivedAlloc ) /= 4) CALL zzrc( 40_4 )

    IF (SIZE(derivedAlloc, 1) /= 1) CALL zzrc( 41_4 )
    IF (SIZE(derivedAlloc, 2) /= 2) CALL zzrc( 42_4 )
    IF (SIZE(derivedAlloc, 3) /= 2) CALL zzrc( 43_4 )
    IF (SIZE(derivedAlloc, 4) /= 1) CALL zzrc( 44_4 )


    PRINT *, 'idxBase    = (', idxBase, ')'
    PRINT *, 'idxDerived = (', idxDerived, ')'


    DO i = 1, 2
        DO j = 1, 2
            IF (idxBase( j,1,1,i ) /= -88) THEN
                CALL zzrc( (50_4 + (((i - 1) * 2) + j)) )

            ELSE IF (idxDerived( j,1,1,i ) /= -99) THEN
                CALL zzrc( (60_4 + (((i - 1) * 2) + j)) )
            END IF
        END DO
    END DO

END PROGRAM allocatedLBoundDerived03
