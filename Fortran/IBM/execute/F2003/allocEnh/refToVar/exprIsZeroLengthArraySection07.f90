!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : exprIsZeroLengthArraySection07 - variable
!*                               and/or expr Contain References to variable
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : October 18, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of an Extended
!*                               Derived Type (with an Allocated ALLOCATABLE
!*                               Component),
!*  SECONDARY FUNCTIONS TESTED : and expr is a Zero Length Array Section of
!*                               variable
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
!*  Both variable and expr may contain references to any portion of variable.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mBase

    TYPE :: tBase
        REAL(16) :: base
    END TYPE tBase

END MODULE mBase

MODULE mDerived1
    USE mBase

    TYPE, EXTENDS(tBase) :: tDerived1
        REAL(8) :: derived1
    END TYPE tDerived1

END MODULE mDerived1

MODULE mDerived2
    USE mDerived1

    TYPE, EXTENDS(tDerived1) :: tDerived2
        REAL(4), ALLOCATABLE :: derived2
    END TYPE tDerived2

END MODULE mDerived2

MODULE mModule
    USE mDerived2

    TYPE(tDerived2), ALLOCATABLE :: d2Alloc( :,:,:,: )

END MODULE mModule

PROGRAM exprIsZeroLengthArraySection07
    USE mModule

    INTERFACE
        SUBROUTINE CheckIt(sizeList, lBoundList, uBoundList, failRC)
            USE mModule
            INTEGER :: sizeList( : )
            INTEGER :: lBoundList( : )
            INTEGER :: uBoundList( : )
            INTEGER(4) :: failRC
        END SUBROUTINE CheckIt
    END INTERFACE

    CALL AllocateIt( )
    CALL CheckIt((/ 10,10,10,10 /), (/ 1,1,1,1 /), (/ 10,10,10,10 /), 10_4)

    CALL AssignIt( )
    CALL CheckIt((/ 10,0,0,10 /), (/ 1,1,1,1 /), (/ 10,0,0,10 /), 20_4)

END PROGRAM exprIsZeroLengthArraySection07

SUBROUTINE AllocateIt( )
    USE mModule

    ALLOCATE( d2Alloc( 10,10,10,10 ) )

    DO i = 1, 10
        DO j = 1, 10
            DO k = 1, 10
                DO l = 1, 10
                    d2Alloc( l,k,j,i ) = tDerived2(REAL(l,16),REAL(k,8),NULL())
                    d2Alloc( l,k,j,i )%derived2 = REAL(j, 4)
                END DO
            END DO
        END DO
    END DO

END SUBROUTINE AllocateIt

SUBROUTINE CheckIt(sizeList, lBoundList, uBoundList, failRC)
    USE mModule

    INTEGER :: sizeList( : )
    INTEGER :: lBoundList( : )
    INTEGER :: uBoundList( : )
    INTEGER(4) :: failRC

    INTEGER(4) :: i

    IF (.NOT. ALLOCATED( d2Alloc )) CALL zzrc( failRC )

    DO i = 1_4, SIZE( sizeList )
        PRINT *, (failRC / 10_4), ')', i,&
                  'SIZE()', SIZE(d2Alloc, i), sizeList( i ),&
                  'LBOUND()', LBOUND(d2Alloc, i), lBoundList( i ),&
                  'UBOUND()', UBOUND(d2Alloc, i), uBoundList( i )

        IF (SIZE(d2Alloc, i) /= sizeList( i ))      CALL zzrc( (failRC + 1_4) )

        IF (LBOUND(d2Alloc, i) /= lBoundList( i ))  CALL zzrc( (failRC + 2_4) )
        IF (UBOUND(d2Alloc, i) /= uBoundList( i ))  CALL zzrc( (failRC + 3_4) )
    END DO

END SUBROUTINE CheckIt

SUBROUTINE AssignIt( )
    USE mModule

    d2Alloc = d2Alloc( :,5:1,10:5,: )

END SUBROUTINE AssignIt
