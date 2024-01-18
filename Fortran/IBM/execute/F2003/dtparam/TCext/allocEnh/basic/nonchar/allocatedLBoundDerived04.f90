! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=none /tstdev/F2003/allocEnh/basic/nonchar/allocatedLBoundDerived04.f
! opt variations: -qnol -qnodeferredlp -qreuse=base

!*  ===================================================================
!*
!*                               Non-CHARACTER Derived Type
!*
!*  DATE                       : August 28, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Derived Type
!*  SECONDARY FUNCTIONS TESTED : and LBOUND(expr) returns a value other than
!*                               "1" as the Lower Bound for an N-Dimension
!*                               Array (1<= N <= 7) -- N == 6
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

MODULE mModule

    TYPE tType(N1,K1)    ! (20,4)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
        INTEGER(K1)   :: i
    END TYPE tType

    TYPE, EXTENDS(tType) :: dType(N2,K2)    ! (20,4,20,4)
        INTEGER, KIND :: K2
        INTEGER, LEN  :: N2
        INTEGER(K2)   :: j
    END TYPE dType

    TYPE, EXTENDS(dType) :: d2Type(N3,K3)    ! (20,4,20,4,20,4)
        INTEGER, KIND :: K3
        INTEGER, LEN  :: N3
        INTEGER(K3)   :: k
    END TYPE d2Type

    TYPE(d2Type(:,4,:,4,:,4)), ALLOCATABLE :: d2AllocArray( :,:,:,:,:,: )

    CONTAINS

        SUBROUTINE CheckSize(n, lBounds, failRCBase)
            INTEGER :: n
            INTEGER :: lBounds( 6 )
            INTEGER(4) :: failRCBase

            INTEGER(4) :: i

            IF (SIZE( d2AllocArray ) /= (n ** 6)) THEN
                PRINT *, "SIZE( d2AllocArray ) =", SIZE( d2AllocArray )
                CALL zzrc( failRCBase )
            END IF

            DO i = 1_4, 6_4
                IF (SIZE(d2AllocArray, i) /= n) THEN
                    PRINT *,"SIZE(d2AllocArray,", i, ") =",SIZE(d2AllocArray,i)
                    CALL zzrc( (failRCBase + i) )

                ELSE IF (LBOUND(d2AllocArray, i) /= lBounds( i )) THEN
                    PRINT *, "LBOUND(d2AllocArray,", i, ") =",&
                                            LBOUND(d2AllocArray, i)
                    PRINT *, "Expecting:", lBounds( i )

                    CALL zzrc( (failRCBase + 10_4 + i) )
                END IF
            END DO

        END SUBROUTINE CheckSize

END MODULE mModule

PROGRAM allocatedLBoundDerived04
    USE mModule

    TYPE(d2Type(20,4,20,4,20,4)) :: d2StaticArray( -5:-4,-3:-2,-1:0,0:1,2:3,4:5 ) =&
      RESHAPE((/ (d2Type(20,4,20,4,20,4)((i + 1),i,(i - 1)), i = 1, 64) /), (/ 2,2,2,2,2,2 /))

    ALLOCATE(d2AllocArray( -5:-3,-4:-2,-3:-1,-2:0,-1:1,0:2 ),&
        SOURCE=RESHAPE((/ (d2Type(20,4,20,4,20,4)((i - 1),i,(i + 1)), i = 1, 729) /),&
                                                        (/ 3,3,3,3,3,3 /)))

    IF (.NOT. ALLOCATED( d2AllocArray ))    CALL zzrc( 10_4 )

    CALL CheckSize(3, (/ -5,-4,-3,-2,-1,0 /), 20_4)

    CALL Assign( )

    CONTAINS

        SUBROUTINE Assign( )

            INTEGER(4) :: val = 0_4

            d2AllocArray = d2StaticArray

            IF (.NOT. ALLOCATED( d2AllocArray ))    CALL zzrc( 40_4 )

            CALL CheckSize(2, (/ -5,-3,-1,0,2,4 /), 50_4)

            DO i = 4, 5
                DO j = 2, 3
                    DO k = 0, 1
                        DO l = -1, -0
                            DO m = -3, -2
                                DO n = -5, -4
                                    val = val + 1_4

                                    PRINT *, d2AllocArray( n,m,l,k,j,i )%i,&
                                             d2AllocArray( n,m,l,k,j,i )%j,&
                                             d2AllocArray( n,m,l,k,j,i )%k, val

                                    IF (d2AllocArray( n,m,l,k,j,i )%i&
                                       /= (val + 1)) CALL zzrc( (100_4 + val) )
                                    IF (d2AllocArray( n,m,l,k,j,i )%j&
                                        /= val)      CALL zzrc( (100_4 + val) )
                                    IF (d2AllocArray( n,m,l,k,j,i )%k&
                                       /= (val - 1)) CALL zzrc( (100_4 + val) )
                                END DO
                            END DO
                        END DO
                    END DO
                END DO
            END DO

        END SUBROUTINE Assign

END PROGRAM allocatedLBoundDerived04
