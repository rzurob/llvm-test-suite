!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-pointer is self-reference of rank-2 w bounds-remapping-list
!* - check if target and each parent component of each element of target
!*   and finalized correctly.
!* - dynamic type of pointer is different from declared type
!*
!234567890123456789012345678901234567890123456789012345678901234567890


 module m

    integer :: counta2 =0, countA0 = 0, countB = 0

    type A

	contains
	    final :: fA_rk_2, fA_scalar
    end type

    type, extends(A) :: B
   	contains
	    final :: fB_rk_2
    end type

    contains
	subroutine fa_rk_2(a)
	    type(A), intent(inout) :: a(:,:)
	    counta2 = counta2 + 1
        end subroutine

	subroutine fa_scalar(a)
	    type(A), intent(inout) :: a
	    countA0 = countA0 + 1
        end subroutine

	subroutine fB_rk_2(a)
	    type(B), intent(inout) :: a(:,:)
	    countB = countB + 1
        end subroutine

 end module

 program main
    use m

    class(A), pointer :: p(:,:)

    allocate(B :: P(3,5))
    p(3:, 5:) => p

    if ( .not. associated(p)) error stop 9
    deallocate(p)

    if ( associated(p) ) error stop 11
    if ( countB /= 1 ) error stop 21
    if ( counta2 /= 0 ) error stop 31
    if ( counta0 /= 15 ) error stop 41

 End program
