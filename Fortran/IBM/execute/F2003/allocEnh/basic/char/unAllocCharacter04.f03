!*  ===================================================================
!*
!*                               Intrinsic Type
!*
!*  DATE                       : September 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Unallocated ALLOCATABLE Array of Type
!*                               CHARACTER
!*  SECONDARY FUNCTIONS TESTED : and expr is a Array of Type CHARACTER with a
!*                               different Length Type Parameter, but the same
!*                               Shape
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
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

PROGRAM unAllocCharacter04

    INTEGER(4) :: i

    CHARACTER(3) :: chrArr3( 5 ) = (/ 'abc', 'def', 'ghi', 'jkl', 'mno' /)
    CHARACTER(5), ALLOCATABLE :: chrArrAlloc( : )


    IF ( ALLOCATED( chrArrAlloc ) ) ERROR STOP 10_4

    chrArrAlloc = chrArr3

    IF (.NOT. ALLOCATED( chrArrAlloc )) ERROR STOP 11_4
    IF (SIZE( chrArrAlloc ) /= 5)       ERROR STOP 12_4

    DO i = 1, 5
        PRINT *, LEN( chrArrAlloc( i ) ), 'chrArrAlloc(', i, ') = "',&
                                chrArrAlloc( i ), '" (', chrArr3( i ), ')'

        IF (LEN( chrArrAlloc( i ) ) /= 5)       CALL zzrc( (14_4 + i) )
        IF (chrArrAlloc( i ) /= chrArr3( i ))   CALL zzrc( (20_4 + i) )
    END DO


    DEALLOCATE( chrArrAlloc )
    PRINT *


    CALL Assign( )


    CONTAINS

        SUBROUTINE Assign( )

            CHARACTER(7) :: chrArr7( 3 ) =&
                (/ 'ABCDEFG', 'HIJKLMN', 'OPQRSTU' /)


            IF ( ALLOCATED( chrArrAlloc ) ) ERROR STOP 30_4

            chrArrAlloc = chrArr7

            IF (.NOT. ALLOCATED( chrArrAlloc )) ERROR STOP 31_4
            IF (SIZE( chrArrAlloc ) /= 3)       ERROR STOP 32_4

            DO i = 1, 3
                PRINT *, LEN( chrArrAlloc( i ) ), 'chrArrAlloc(', i, ') = "',&
                                    chrArrAlloc( i ), '" (', chrArr7( i ), ')'

                IF (LEN( chrArrAlloc( i ) ) /= 5) CALL zzrc( (34_4 + i) )
                IF (chrArrAlloc( i ) /= chrArr7( i )( :5 ))&
                                            CALL zzrc( (40_4 + i) )
            END DO

        END SUBROUTINE Assign

END PROGRAM unAllocCharacter04
