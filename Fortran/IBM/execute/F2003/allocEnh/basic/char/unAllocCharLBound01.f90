!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : unAllocCharLBound01 - Basic Tests: CHARACTER
!*                               Intrinsic Type
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : September 13, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Unallocated ALLOCATABLE Array of Type
!*                               CHARACTER
!*  SECONDARY FUNCTIONS TESTED : and LBOUND(expr) returns a value other than
!*                               "1" as the Lower Bound for a single Dimension
!*                               Array
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

PROGRAM unAllocCharLBound01

    INTEGER(4) :: i

    CHARACTER(4) :: charValue
    CHARACTER(4), ALLOCATABLE :: chrArrAlloc( : )


    IF ( ALLOCATED( chrArrAlloc ) )     CALL zzrc( 10_4 )

    CALL IntrinsicAssignment( )

    IF (.NOT. ALLOCATED( chrArrAlloc )) CALL zzrc( 20_4 )


    PRINT *, "SIZE( chrArrAlloc ) =", SIZE( chrArrAlloc ),&
                ", LBOUND(chrArrAlloc, 1) =", LBOUND(chrArrAlloc, 1)

    IF (SIZE( chrArrAlloc ) /= 11)      CALL zzrc( 30_4 )
    IF (LBOUND(chrArrAlloc, 1) /= -5)   CALL zzrc( 31_4 )


    DO i = -5, 5
        WRITE(charValue, '(I4)') (1000_4 - i)

        PRINT 10, i, chrArrAlloc( i ), charValue, LEN( chrArrAlloc( i ) )
10      FORMAT("chrArrAlloc(",I2,") = '",A4,"' (",A4,") (",I5,")")

        IF (LEN( chrArrAlloc( i ) ) /= 4)   CALL zzrc( (40_4 + i) )
        IF (chrArrAlloc( i ) /= charValue)  CALL zzrc( (60_4 + i) )
    END DO


    CONTAINS

        SUBROUTINE IntrinsicAssignment( )

            INTEGER(4) :: i
            CHARACTER(4) :: chrArr( -5:5 )


            DO i = -5, 5
                WRITE(chrArr( i ), '(I4)') (1000_4 - i)
            END DO

            chrArrAlloc = chrArr

        END SUBROUTINE IntrinsicAssignment

END PROGRAM unAllocCharLBound01
