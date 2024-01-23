!*  ===================================================================
!*
!*                               Intrinsic Type
!*
!*  DATE                       : September 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Unallocated ALLOCATABLE Scalar of Type
!*                               CHARACTER
!*  SECONDARY FUNCTIONS TESTED : and expr is a Scalar of Type CHARACTER with a
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

MODULE mModule

    CHARACTER(2) :: scalarChar2 = 'Tt'
    CHARACTER(4), ALLOCATABLE :: scalarCharAlloc

    CONTAINS


        SUBROUTINE Char2Assign( )

            IF ( ALLOCATED( scalarCharAlloc ) )     ERROR STOP 10_4

            scalarCharAlloc = scalarChar2

            IF (.NOT. ALLOCATED( scalarCharAlloc )) ERROR STOP 11_4

            PRINT *, '(', scalarCharAlloc, ') (',&
                        scalarChar2, ')', LEN( scalarCharAlloc )

            IF (LEN( scalarCharAlloc ) /= 4)        ERROR STOP 12_4
            IF (scalarCharAlloc /= scalarChar2)     ERROR STOP 13_4

        END SUBROUTINE Char2Assign

END MODULE mModule


PROGRAM unAllocCharacter03
    USE mModule

    INTERFACE
        SUBROUTINE Char4Assign( )
            USE mModule
        END SUBROUTINE Char4Assign
    END INTERFACE

    CHARACTER(8) :: scalarChar8 = 'testTEST'


    CALL Char2Assign( )
    DEALLOCATE( scalarCharAlloc )

    CALL Char4Assign( )
    DEALLOCATE( scalarCharAlloc )

    CALL Char8Assign( )
    DEALLOCATE( scalarCharAlloc )


    CONTAINS

        SUBROUTINE Char8Assign( )

            IF ( ALLOCATED( scalarCharAlloc ) )         ERROR STOP 20_4

            scalarCharAlloc = scalarChar8

            IF (.NOT. ALLOCATED( scalarCharAlloc ))     ERROR STOP 21_4

            PRINT *, '(', scalarCharAlloc, ') (',&
                        scalarChar8, ')', LEN( scalarCharAlloc )

            IF (LEN( scalarCharAlloc ) /= 4)            ERROR STOP 22_4
            IF (scalarCharAlloc /= scalarChar8( 1:4 ))  ERROR STOP 23_4

        END SUBROUTINE Char8Assign

END PROGRAM unAllocCharacter03


SUBROUTINE Char4Assign( )
    USE mModule

    CHARACTER(4) :: scalarChar4 = 'TEST'


    IF ( ALLOCATED( scalarCharAlloc ) )     ERROR STOP 30_4

    scalarCharAlloc = scalarChar4

    IF (.NOT. ALLOCATED( scalarCharAlloc )) ERROR STOP 31_4

    PRINT *, '(', scalarCharAlloc, ') (',&
                        scalarChar4, ')', LEN( scalarCharAlloc )

    IF (LEN( scalarCharAlloc ) /= 4)        ERROR STOP 32_4
    IF (scalarCharAlloc /= scalarChar4)     ERROR STOP 33_4

END SUBROUTINE Char4Assign
