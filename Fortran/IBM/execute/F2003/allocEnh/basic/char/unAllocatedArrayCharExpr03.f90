!*  ===================================================================
!*
!*                               CHARACTER Intrinsic Type
!*
!*  DATE                       : September 18, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is
!*                               an Unallocated ALLOCATABLE Array of Type
!*                               CHARACTER
!*  SECONDARY FUNCTIONS TESTED : and expr is a (complex) CHARACTER Array
!*                               Expression of a bigger Size than variable
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

    CHARACTER(7), ALLOCATABLE :: charArrAlloc( : )

END MODULE mModule

PROGRAM unAllocatedArrayCharExpr03
    USE mModule

    INTERFACE
        SUBROUTINE SubMain( char4Arr )
            USE mModule
            CHARACTER(4) :: char4Arr( : )
        END SUBROUTINE SubMain
    END INTERFACE

    CHARACTER(4) :: c4( 1131:1139 )


    DO i = 1131, 1139
        WRITE(c4( i ), '(I4)') i
    END DO


    CALL SubMain( c4 )


    DO i = 1, 9
        PRINT *, LEN( charArrAlloc( i ) ), "'", charArrAlloc( i ), "'"

        IF (LEN( charArrAlloc( i ) ) /= 7)  CALL zzrc( 30_4 )
        IF (charArrAlloc( i ) /= 'THX-113') CALL zzrc( 31_4 )
    END DO

END PROGRAM unAllocatedArrayCharExpr03

SUBROUTINE SubMain( char4Arr )
    USE mModule

    CHARACTER(4) :: char4Arr( : )

    CHARACTER(3) :: char3Arr( 9 ) = 'THX'


    CALL SubSub( )

    PRINT *, SIZE( charArrAlloc )
    IF (SIZE( charArrAlloc ) /= 9)      CALL zzrc( 20_4 )


    CONTAINS

        SUBROUTINE SubSub( )

            IF ( ALLOCATED( charArrAlloc ) )      CALL zzrc( 10_4 )

            charArrAlloc = char3Arr // '-' // char4Arr

            IF (.NOT. ALLOCATED( charArrAlloc ) )  CALL zzrc( 11_4 )

        END SUBROUTINE SubSub

END SUBROUTINE SubMain
