!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : unAllocatedAllocatable01 - Basic Tests:
!*                               Non-CHARACTER Intrinsic Types
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : August  2, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Unallocated ALLOCATABLE Scalar/Array of
!*                               Intrinsic (Non-CHARACTER) Type
!*  SECONDARY FUNCTIONS TESTED : and expr is a Scalar/Array of Intrinsic Type
!*                               with the same Shape and Length Type Parameter
!*                               Values
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

PROGRAM unAllocatedAllocatable01

    INTEGER(4) :: i
    INTEGER(4) :: j

    INTEGER, PARAMETER :: N = 10

    COMPLEX, PARAMETER :: COMPLEX_SCALAR = ( 4.321,9.876 )
    COMPLEX, ALLOCATABLE :: complexScalarAlloc

    REAL :: realArray( N,N )
    REAL, ALLOCATABLE :: realArrayAlloc( :,: )


    IF ( ALLOCATED( complexScalarAlloc ) )  CALL zzrc( 10_4 )
    IF ( ALLOCATED( realArrayAlloc ) )      CALL zzrc( 20_4 )


    complexScalarAlloc = COMPLEX_SCALAR

    IF (.NOT. ALLOCATED( complexScalarAlloc )) CALL zzrc( 30_4 )
    IF (complexScalarAlloc /= ( 4.321,9.876 )) CALL zzrc( 40_4 )


    DO i = 1, N
        DO j = 1, N, 2
            realArray( j,i ) = REAL( complexScalarAlloc )
            realArray( (j + 1),i ) = AIMAG( complexScalarAlloc )
        END DO
    END DO


    realArrayAlloc = realArray

    IF (.NOT. ALLOCATED( realArrayAlloc )) CALL zzrc( 50_4 )

    DO i = N, 1, -1
        DO j = N, 1, -1
            IF (realArrayAlloc( j,i ) /= realArray( j,i )) THEN
                PRINT *, "realArray(", j, ",", i, ") =",&
                                            realArray( j,i )
                PRINT *, "realArrayAlloc(", j, ",", i, ") =",&
                                            realArrayAlloc( j,i )

                CALL zzrc( (100_4 + ((i - 1) * 10) + j) )
            END IF
        END DO
    END DO

END PROGRAM unAllocatedAllocatable01
