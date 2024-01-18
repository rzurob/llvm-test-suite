! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/allocEnh/exprEval/order/arrayExprVectorIndex08.f
! opt variations: -ql

!*  ===================================================================
!*
!*                               Evaluation
!*
!*  DATE                       : October  3, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Derived Type
!*  SECONDARY FUNCTIONS TESTED : expr references variable, and will have a
!*                               different Shape Result (Indexed using a
!*                               Vector Subscript)
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

PROGRAM arrayExprVectorIndex08

    TYPE tType(K1)    ! (4)
        INTEGER, KIND            :: K1
        INTEGER(K1), ALLOCATABLE :: idx( : )
    END TYPE tType

    INTEGER :: idxNum = 10

    TYPE(tType(4)), ALLOCATABLE :: typeArrAlloc( :,: )


    ALLOCATE( typeArrAlloc( 10,10 ) )


    idxNum = 9
    DO i = 1, 10
        DO j = 1, 10
            ALLOCATE( typeArrAlloc( j,i )%idx( (idxNum - 1) ) )

            DO k = 2, idxNum
                typeArrAlloc( j,i )%idx( (k - 1) ) = k
            END DO
        END DO

        idxNum = idxNum - 2
    END DO


    DO i = 1, 4
        IF (MOD(i, 2) == 0) THEN
            typeArrAlloc = typeArrAlloc( typeArrAlloc( 1,1 )%idx,: )
        ELSE
            typeArrAlloc = typeArrAlloc( :,typeArrAlloc( 1,1 )%idx )
        END IF


        PRINT *

        DO j = 1, SIZE(typeArrAlloc, 2)
            DO k = 1, SIZE(typeArrAlloc, 1)
                PRINT *, k, j, '(', typeArrAlloc( k,j )%idx, ')'
            END DO
        END DO
    END DO

END PROGRAM arrayExprVectorIndex08
