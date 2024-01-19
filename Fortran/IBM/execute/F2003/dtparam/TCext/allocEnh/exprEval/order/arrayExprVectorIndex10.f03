! GB DTP extension using:
! ftcx_dtp -qck -qdeferredlp /tstdev/F2003/allocEnh/exprEval/order/arrayExprVectorIndex10.f
! opt variations: -qnock -qnodeferredlp -qreuse=self

!*  ===================================================================
!*
!*                               Evaluation
!*
!*  DATE                       : October  4, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Derived Type
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

MODULE mCell

    TYPE tCell(K1,N1)    ! (1,1)
        INTEGER, KIND             :: K1
        INTEGER, LEN              :: N1
        CHARACTER(kind=K1,len=N1) :: marker
    END TYPE tCell

    TYPE(tCell(1,:)), ALLOCATABLE :: board( :,:,: )

END MODULE mCell


MODULE mMove
    USE mCell

    TYPE, EXTENDS(tCell) :: tMove(K2,K3,K4)    ! (1,1,4,4,4)
        INTEGER, KIND :: K2,K3,K4
        INTEGER(K2)   :: x
        INTEGER(K3)   :: y
        INTEGER(K4)   :: n
    END TYPE tMove

END MODULE mMove


PROGRAM arrayExprVectorIndex10
    USE mMove

    INTERFACE
        SUBROUTINE Move( play )
            USE mMove
            TYPE(tMove(1,*,4,4,4)) :: play
        END SUBROUTINE Move
    END INTERFACE


    CHARACTER(1), PARAMETER :: X = 'x'
    CHARACTER(1), PARAMETER :: O = 'o'

    TYPE(tMove(1,1,4,4,4)), PARAMETER :: moveList( 9 ) =&
        (/  tMove(1,1,4,4,4)(X,1,1,1), tMove(1,1,4,4,4)(O,3,3,2), tMove(1,1,4,4,4)(X,3,1,3),&
            tMove(1,1,4,4,4)(O,2,1,4), tMove(1,1,4,4,4)(X,2,3,5), tMove(1,1,4,4,4)(O,1,2,6),&
            tMove(1,1,4,4,4)(X,3,2,7), tMove(1,1,4,4,4)(O,2,2,8), tMove(1,1,4,4,4)(X,1,3,9) /)


    ALLOCATE( tCell(1,1) :: board( 3,3,1 ) )
    board = tCell(1,1)(' ')


    DO i = 1, 9
        CALL Move( moveList( i ) )
    END DO


    DO i = SIZE(board, 3), 1, -1
        DO j = 1, 3
            PRINT 10, (board( k,j,i )%marker, k = 1, 3)

            IF (j < 3) THEN
                PRINT 20

            ELSE IF (i > 1) THEN
                PRINT *
            END IF

10          FORMAT(A1,"|",A1,"|",A1)
20          FORMAT("-+-+-")
        END DO
    END DO

END PROGRAM arrayExprVectorIndex10


SUBROUTINE Move( play )
    USE mMove

    TYPE(tMove(1,*,4,4,4)) :: play

    board( play%x,play%y,1 )%marker = play%marker

    IF (play%n /= 9) THEN
        board = board( :,:,(/ 1, (/ (i, i = 1, SIZE(board, 3)) /) /) )
    END IF

END SUBROUTINE Move
