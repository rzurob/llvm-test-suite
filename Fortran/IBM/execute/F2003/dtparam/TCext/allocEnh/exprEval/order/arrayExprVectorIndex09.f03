! GB DTP extension using:
! ftcx_dtp -qck -qdeferredlp -qreuse=self /tstdev/F2003/allocEnh/exprEval/order/arrayExprVectorIndex09.f
! opt variations: -qnock -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*                               Evaluation
!*
!*  DATE                       : October  4, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Derived Type
!*                               (Indexed using a Vector Subscript)
!*  SECONDARY FUNCTIONS TESTED : expr references variable, and will have a
!*                               different Shape Result (Indexed using a
!*                               Vector Subscript)
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

    TYPE :: tType(K1,N1,K2,N2)    ! (1,1,4,4)
        INTEGER, KIND             :: K1,K2
        INTEGER, LEN              :: N1,N2
        CHARACTER(kind=K1,len=N1) :: id
        INTEGER(K2)               :: n
        INTEGER(K2)               :: subScripts( N2 )
    END TYPE tType


    TYPE(tType(1,:,4,:)), ALLOCATABLE :: v( :,:,: )

END MODULE mModule


PROGRAM arrayExprVectorIndex09

    INTERFACE
        SUBROUTINE Alloc( )
            USE mModule
        END SUBROUTINE Alloc

        SUBROUTINE ExpandByOne( )
            USE mModule
        END SUBROUTINE ExpandByOne

        SUBROUTINE DumpV( )
            USE mModule
        END SUBROUTINE DumpV
    END INTERFACE


    CALL Alloc( )
    CALL DumpV( )

    PRINT *

    CALL ExpandByOne( )
    CALL DumpV( )

END PROGRAM arrayExprVectorIndex09


SUBROUTINE Alloc( )
    USE mModule


    ALLOCATE(v( 3,3,3 ), SOURCE=RESHAPE((/  (((&
        tType(1,1,4,4)(CHAR( ((i * 9) + (j * 3) + k + ICHAR( 'A' ))),3,(/ 3,2,1,1 /)),&
                              k = 0, 2), j = 0, 2), i = 0, 2) /), (/ 3,3,3 /)))

    v( 3,3,3 )%id = 'a'

END SUBROUTINE Alloc


SUBROUTINE ExpandByOne( )
    USE mModule


    v( v( 1,1,1 )%subScripts( v( 1,1,1 )%n):v( 1,1,1 )%subScripts(1) ,:,: ) =&
                            v( :,:,v( 1,1,1 )%subScripts( :3 ) )

END SUBROUTINE ExpandByOne


SUBROUTINE DumpV( )
    USE mModule


    DO j = 1, SIZE(v, 2)
        PRINT *, (v( k,j,1 )%id, k = 1, SIZE(v, 3)), ' ',&
                 (v( k,j,2 )%id, k = 1, SIZE(v, 3)), ' ',&
                 (v( k,j,3 )%id, k = 1, SIZE(v, 3))
    END DO

END SUBROUTINE DumpV
