! GB DTP extension using:
! ftcx_dtp -qk -qdeferredlp -qreuse=base /tstdev/F2003/allocEnh/exprEval/order/arrayExprVectorIndex07.f
! opt variations: -qck -qnok -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*                               Evaluation
!*
!*  DATE                       : October  3, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is
!*                               an Allocated ALLOCATABLE Array of Derived
!*                               Type (Indexed using a Vector Subscript)
!*  SECONDARY FUNCTIONS TESTED : expr references variable, and will have a
!*                               different Shape Result
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

PROGRAM arrayExprVectorIndex07

    TYPE tBase(K1,N1)    ! (4,1)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
        CHARACTER(N1) :: id
    END TYPE tBase

    TYPE, EXTENDS(tBase) :: tDerived(N2)    ! (4,1,10)
        INTEGER, LEN :: N2
        INTEGER(K1)  :: length
        INTEGER(K1)  :: idx( N2 )
    END TYPE tDerived


    TYPE(tDerived(4,:,:)), ALLOCATABLE :: tDerivedAlloc( : )


    ALLOCATE(tDerivedAlloc( 10 ),&
        SOURCE=(/   tDerived(4,1,10)('a',10,(/   (i, i = 10, 1, -1)   /)),&
                    tDerived(4,1,10)('b', 0,(/     (0, i = 1, 10)     /)),&
                    tDerived(4,1,10)('c', 6,(/  8,7,6,5,4,3, 0,0,0,0  /)),&
                    tDerived(4,1,10)('d', 0,(/     (0, i = 1, 10)     /)),&
                    tDerived(4,1,10)('e', 2,(/   6,5, (0, i = 1, 8)   /)),&
                    tDerived(4,1,10)('E', 0,(/     (0, i = 1, 10)     /)),&
                    tDerived(4,1,10)('D', 4,(/ 7,6,5,4, (0, i = 1, 6) /)),&
                    tDerived(4,1,10)('C', 0,(/     (0, i = 1, 10)     /)),&
                    tDerived(4,1,10)('B', 8,(/ (i, i = 9, 2, -1), 0,0 /)),&
                    tDerived(4,1,10)('A', 0,(/     (0, i = 1, 10)     /)) /))


    PRINT *, 0, (tDerivedAlloc( j )%id, j = 1, 10)


    DO i = 1, 5
        tDerivedAlloc( tDerivedAlloc(i)%idx(1):tDerivedAlloc(i)%idx(tDerivedAlloc( i )%length):-1 )&
            = tDerivedAlloc( i:11-i)

        PRINT *, i, (tDerivedAlloc( j )%id, j = 1, 10)
    END DO

END PROGRAM arrayExprVectorIndex07
