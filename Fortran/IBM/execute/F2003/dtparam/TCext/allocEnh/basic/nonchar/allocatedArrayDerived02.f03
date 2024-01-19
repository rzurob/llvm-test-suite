! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/allocEnh/basic/nonchar/allocatedArrayDerived02.f
! opt variations: -ql -qreuse=none

!*  ===================================================================
!*
!*                               Non-CHARACTER Derived Type
!*
!*  DATE                       : August  8, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Derived Type
!*  SECONDARY FUNCTIONS TESTED : and expr has a different Shape (but the same
!*                               Length Type Parameter Values)
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

MODULE mBaseModule

    TYPE tTypeBase(K1)    ! (4)
        INTEGER, KIND :: K1
        INTEGER(K1)   :: i
    END TYPE tTypeBase

END MODULE mBaseModule


MODULE mDerivedModule
    USE mBaseModule

    TYPE, EXTENDS(tTypeBase) :: tTypeDerived    ! (4)
        INTEGER(K1) :: j

        CONTAINS

            FINAL :: tTypeDerivedFinal

    END TYPE tTypeDerived

    CONTAINS

        SUBROUTINE tTypeDerivedFinal( o )
            TYPE(tTypeDerived(4)) :: o( :,:,: )

            PRINT '("tTypeDerivedFinal( (",I2,",",I2,") )")',&
                                        o( 1,1,1 )%i, o( 1,1,1 )%j

        END SUBROUTINE tTypeDerivedFinal

END MODULE mDerivedModule


PROGRAM allocatedArrayDerived02
    USE mDerivedModule

    INTEGER(4) :: i

    TYPE(tTypeDerived(4)) :: derivedArray( 2,2,2 ) =&
        RESHAPE((/ (tTypeDerived(4)( i,(8 - i) ), i = 1, 8) /), (/ 2,2,2 /))

    TYPE(tTypeDerived(4)), ALLOCATABLE :: derivedArrayAlloc( :,:,: )


    ALLOCATE(derivedArrayAlloc( 3,3,3 ),&
        SOURCE=RESHAPE((/ (tTypeDerived(4)( (27 - i),i ), i = 27, 1, -1) /),&
                                                                (/ 3,3,3 /)))

    IF (.NOT. ALLOCATED( derivedArrayAlloc ))   CALL zzrc( 10_4 )
    IF (SIZE( derivedArrayAlloc ) /= 27) CALL zzrc( 20_4 )

    DO i = 1, 3
        IF (SIZE(derivedArrayAlloc, i) /= 3) CALL zzrc( (20_4 + i) )
    END DO

	derivedArrayAlloc( 1,1,1 )%i = -1
	derivedArrayAlloc( 1,1,1 )%j = 99


    CALL AssignIt( )


    IF (.NOT. ALLOCATED( derivedArrayAlloc ))   CALL zzrc( 100_4 )
    IF (SIZE( derivedArrayAlloc ) /= 8) CALL zzrc( 120_4 )

    DO i = 1, 3
        IF (SIZE(derivedArrayAlloc, i) /= 2) CALL zzrc( (120_4 + i) )
    END DO


    CONTAINS

        SUBROUTINE AssignIT( )

            derivedArrayAlloc = derivedArray

        END SUBROUTINE AssignIT

END PROGRAM allocatedArrayDerived02
