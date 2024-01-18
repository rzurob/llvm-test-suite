!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : allocCharLBound01 - Basic Tests: CHARACTER
!*                               Intrinsic Type
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : September  8, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Type CHARACTER
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

MODULE mModule
    CHARACTER(6), ALLOCATABLE :: chArrAlloc1( : )
    CHARACTER(:), ALLOCATABLE :: chArrAlloc2( : )
END MODULE mModule


PROGRAM allocCharLBound01
    USE mModule

    INTEGER(4) :: i


    ALLOCATE(chArrAlloc1( 0:6 ), SOURCE=(/ ('ABCDEF', i = 1, 7) /))
    IF (.NOT. ALLOCATED( chArrAlloc1 )) CALL zzrc( 10_4 )

    CALL IntrinsicAssignment1( )

    IF (.NOT. ALLOCATED( chArrAlloc1 )) CALL zzrc( 11_4 )
    IF (SIZE( chArrAlloc1 ) /= 11)      CALL zzrc( 12_4 )
    IF (LBOUND(chArrAlloc1, 1) /= -5)   CALL zzrc( 13_4 )


    ALLOCATE(CHARACTER(7) :: chArrAlloc2( -2:0 ))
    IF (.NOT. ALLOCATED( chArrAlloc2 )) CALL zzrc( 20_4 )

    chArrAlloc2 = (/ 'ABCDEFG', 'HIJKLMN', 'OPQRSTU' /)

    CALL IntrinsicAssignment2( )

    IF (.NOT. ALLOCATED( chArrAlloc2 )) CALL zzrc( 21_4 )
    IF (SIZE( chArrAlloc2 ) /= 3)       CALL zzrc( 22_4 )
    IF (LBOUND(chArrAlloc2, 1) /= -1)   CALL zzrc( 23_4 )


    DO i = -5, 5
        IF (LEN( chArrAlloc1( i ) ) /= 6) THEN
            PRINT *, "LEN( chArrAlloc1(", i, ") =", LEN( chArrAlloc1( i ) )
            CALL zzrc( (35_4 + i) )

        ELSE IF (chArrAlloc1( i ) /= 'abcdef') THEN
            WRITE(6,10) 'chArrAlloc1', i, chArrAlloc1( i )
            CALL zzrc( (55_4 + i) )

        ELSE IF ((i < 2) .AND. (i > -2)) THEN
            IF (LEN( chArrAlloc2( i ) ) /= 13) THEN
                PRINT *,"LEN( chArrAlloc2(", i, ") =",LEN( chArrAlloc2( i ) )
                CALL zzrc( (71_4 + i) )

            ELSE IF (chArrAlloc2( i ) /= 'abcdefghijklm') THEN
                WRITE(6,20) 'chArrAlloc2', i, chArrAlloc2( i )
                CALL zzrc( (81_4 + i) )
            END IF
        END IF
    END DO

10  FORMAT(A11,'(',I2,') = "',A6,'"')
20  FORMAT(A11,'(',I2,') = "',A13,'"')

END PROGRAM allocCharLBound01


SUBROUTINE IntrinsicAssignment1( )
    USE mModule

    CHARACTER(13) :: chArr( -5:5 ) = (/ ('abcdefghijklm', i = 1, 11) /)

    chArrAlloc1 = chArr

END SUBROUTINE IntrinsicAssignment1


SUBROUTINE IntrinsicAssignment2( )
    USE mModule

    CHARACTER(13) :: chArr( -1:1 ) = (/ ('abcdefghijklm', i = 1, 3) /)

    chArrAlloc2 = chArr

END SUBROUTINE IntrinsicAssignment2
