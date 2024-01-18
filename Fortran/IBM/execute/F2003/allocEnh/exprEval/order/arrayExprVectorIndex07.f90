!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : arrayExprVectorIndex07 - Order of Expression
!*                               Evaluation
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : October  3, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is
!*                               an Allocated ALLOCATABLE Array of Derived
!*                               Type (Indexed using a Vector Subscript)
!*  SECONDARY FUNCTIONS TESTED : expr references variable, and will have a
!*                               different Shape Result
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

PROGRAM arrayExprVectorIndex07

    TYPE tBase
        CHARACTER(1) :: id
    END TYPE tBase

    TYPE, EXTENDS(tBase) :: tDerived
        INTEGER :: length
        INTEGER :: idx( 10 )
    END TYPE tDerived


    TYPE(tDerived), ALLOCATABLE :: tDerivedAlloc( : )


    ALLOCATE(tDerivedAlloc( 10 ),&
        SOURCE=(/   tDerived('a',10,(/   (i, i = 10, 1, -1)   /)),&
                    tDerived('b', 0,(/     (0, i = 1, 10)     /)),&
                    tDerived('c', 6,(/  8,7,6,5,4,3, 0,0,0,0  /)),&
                    tDerived('d', 0,(/     (0, i = 1, 10)     /)),&
                    tDerived('e', 2,(/   6,5, (0, i = 1, 8)   /)),&
                    tDerived('E', 0,(/     (0, i = 1, 10)     /)),&
                    tDerived('D', 4,(/ 7,6,5,4, (0, i = 1, 6) /)),&
                    tDerived('C', 0,(/     (0, i = 1, 10)     /)),&
                    tDerived('B', 8,(/ (i, i = 9, 2, -1), 0,0 /)),&
                    tDerived('A', 0,(/     (0, i = 1, 10)     /)) /))


    PRINT *, 0, (tDerivedAlloc( j )%id, j = 1, 10)


    DO i = 1, 5
        tDerivedAlloc( tDerivedAlloc(i)%idx(1):tDerivedAlloc(i)%idx(tDerivedAlloc( i )%length ):-1 )&
            = tDerivedAlloc(i:11-i)

        PRINT *, i, (tDerivedAlloc( j )%id, j = 1, 10)
    END DO

END PROGRAM arrayExprVectorIndex07
