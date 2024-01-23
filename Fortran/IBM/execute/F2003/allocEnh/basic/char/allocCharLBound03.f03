!*  ===================================================================
!*
!*                               Intrinsic Type
!*
!*  DATE                       : September 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Type CHARACTER
!*  SECONDARY FUNCTIONS TESTED : and LBOUND(expr) returns a value other than
!*                               "1" as the Lower Bound for an N-Dimension
!*                               Array (1<= N <= 7) -- N == 5
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

MODULE mModule

    CHARACTER(:), ALLOCATABLE :: chrArrAlloc1( :,:,:,:,: )
    CHARACTER(1), ALLOCATABLE :: chrArrAlloc2( :,:,:,:,: )

END MODULE mModule


PROGRAM allocCharLBound03
    USE mModule

    INTERFACE
        SUBROUTINE IntrinsicAssignment( )
            USE mModule
        END SUBROUTINE IntrinsicAssignment
    END INTERFACE

    INTEGER(4) :: i


    ALLOCATE(CHARACTER(3) :: chrArrAlloc1( -1:1,-1:1,-1:1,-1:1,-1:1 ))

    IF (.NOT. ALLOCATED( chrArrAlloc1 )) ERROR STOP 10_4
    chrArrAlloc1 = RESHAPE((/ ('ibm', i = 1, 243) /), (/ 3,3,3,3,3 /))


    ALLOCATE(chrArrAlloc2( -1:1,-1:1,-1:1,-1:1,-1:1 ),&
        SOURCE=RESHAPE((/ (CHAR( (32 + MOD(i, 94)) ), i = 1, 243) /),&
                                                        (/ 3,3,3,3,3 /)))
    IF (.NOT. ALLOCATED( chrArrAlloc2 )) ERROR STOP 11_4


    CALL IntrinsicAssignment( )


    IF (.NOT. ALLOCATED( chrArrAlloc1 )) ERROR STOP 20_4
    IF (.NOT. ALLOCATED( chrArrAlloc2 )) ERROR STOP 21_4

    IF (SIZE( chrArrAlloc1 ) /= 32) ERROR STOP 30_4
    IF (SIZE( chrArrAlloc2 ) /= 32) ERROR STOP 31_4


    DO i = 1, 5
        IF (SIZE(chrArrAlloc1, i) /= 2) CALL zzrc( (40_4 + i) )
        IF (SIZE(chrArrAlloc2, i) /= 2) CALL zzrc( (45_4 + i) )

        IF (LBOUND(chrArrAlloc1, i) /= 0) CALL zzrc( (50_4 + i) )
        IF (LBOUND(chrArrAlloc2, i) /= 0) CALL zzrc( (55_4 + i) )
    END DO


    IF (.NOT. ALL(chrArrAlloc1 == 'ibmtest')) THEN
        WRITE(6, *) 'chrArrAlloc1:'
        WRITE(6, 10) chrArrAlloc1
10      FORMAT(2('"',A7,'","',A7,'", '))

        ERROR STOP 60_4

    ELSE IF (.NOT. ALL(chrArrAlloc2 == 'i')) THEN
        WRITE(6, *) 'chrArrAlloc2:'
        WRITE(6, 20) chrArrAlloc2
20      FORMAT(2('"',A1,'","',A1,'", '))

        ERROR STOP 70_4
    END IF

END PROGRAM allocCharLBound03


SUBROUTINE IntrinsicAssignment( )
    USE mModule

    CHARACTER(7) :: chrArr( 0:1,0:1,0:1,0:1,0:1 ) =&
            RESHAPE((/ ('ibmtest', i = 1, 32) /), (/ 2,2,2,2,2 /))

    chrArrAlloc1 = chrArr
    chrArrAlloc2 = chrArr

END SUBROUTINE IntrinsicAssignment
