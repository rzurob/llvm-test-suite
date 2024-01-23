!*  ===================================================================
!*
!*                               Non-CHARACTER Derived Type
!*
!*  DATE                       : August  8, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Derived Type
!*  SECONDARY FUNCTIONS TESTED : and expr is a Array of Derived Type with the
!*                               same Shape and Length Type Parameter Values
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

    TYPE tBaseType
        COMPLEX :: c

        CONTAINS

            FINAL :: tBaseTypeFinal

    END TYPE tBaseType

    CONTAINS

        SUBROUTINE tBaseTypeFinal( o )
            TYPE(tBaseType) :: o( : )

            PRINT "('tBaseTypeFinal( (',F4.1,',',F4.1,') )')", o( 1 )%c

        END SUBROUTINE tBaseTypeFinal

END MODULE mModule

PROGRAM allocatedArrayDerived01
    USE mModule

    TYPE, EXTENDS(tBaseType) :: tDerivedType
          INTEGER :: i
    END TYPE tDerivedType


    INTEGER(4) :: i

    TYPE(tDerivedType) :: derivedType( 10 ) =&
          (/ (tDerivedType( CMPLX(i, -i),i ), i = 1, 10) /)

    TYPE(tDerivedType), ALLOCATABLE :: derivedTypeAlloc( : )


    ALLOCATE(derivedTypeAlloc( 10 ),&
        SOURCE=(/ (tDerivedType( CMPLX(-i, i),i ), i = 10, 1, -1) /))


    IF (.NOT. ALLOCATED( derivedTypeAlloc ))    ERROR STOP 10_4
    IF (SIZE( derivedTypeAlloc ) /= 10)         ERROR STOP 20_4

    DO i = 1, 10
        j = 11 - i

        IF (derivedTypeAlloc( i )%c /= ( -j,j )) THEN
            PRINT *, 'derivedTypeAlloc(', i, ')%c =', derivedTypeAlloc( i )%c
            PRINT *, 'Expected:', CMPLX(j, -j)

            CALL zzrc( (30_4 + i) )

        ELSE IF (derivedTypeAlloc( i )%i /= j) THEN
            PRINT *, 'derivedTypeAlloc(', i, ')%i =', derivedTypeAlloc( i )%i
            PRINT *, 'Expected:', j

            CALL zzrc( (40_4 + i) )
        END IF
    END DO


    derivedTypeAlloc = derivedType


    IF (.NOT. ALLOCATED( derivedTypeAlloc ))    ERROR STOP 50_4
    IF (SIZE( derivedTypeAlloc ) /= 10)         ERROR STOP 60_4

    DO i = 1, 10
        IF (derivedTypeAlloc( i )%c /= ( i,-i )) THEN
            PRINT *, 'derivedTypeAlloc(', i, ')%c =', derivedTypeAlloc( i )%c
            PRINT *, 'Expected:', CMPLX(i, -i)

            CALL zzrc( (70_4 + i) )

        ELSE IF (derivedTypeAlloc( i )%i /= i) THEN
            PRINT *, 'derivedTypeAlloc(', i, ')%i =', derivedTypeAlloc( i )%i
            PRINT *, 'Expected:', i

            CALL zzrc( (80_4 + i) )
        END IF
    END DO

END PROGRAM allocatedArrayDerived01
