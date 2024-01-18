!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc027.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (deallocate a pointer whose target is
!                               not created by allocate statement causes an
!                               error message)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(4) id

        contains

        final :: finalizeBase, finalizeBaseRank1
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base), intent(in) :: b(:)

        print *, 'finalizeBaseRank1'
    end subroutine
end module


program falloc027
use m
    class (base), pointer :: b1_ptr, b2_ptr(:), b3_ptr(:)
    type (base), target :: b1, b2(10), b3(2,2)

    type (base), pointer :: b11_ptr, b21_ptr, b31_ptr(:)
    class (*), pointer :: x1, x2(:), x3(:)

    integer :: err(3, 3) = 0

    !! use the poly-pointer to assign to target not created by ALLOCATE
    b1_ptr => b1

    b2_ptr => b2

    b3_ptr => b3(:,2)

    deallocate (b1_ptr, stat=err(1, 1))
    deallocate (b2_ptr, stat=err(2, 1))
    deallocate (b3_ptr, stat=err(3, 1))


    !! us the nonpoly-pointer to assign to targets not created by allocate
    b11_ptr => b1
    b21_ptr => b2(1)
    b31_ptr => b2

    deallocate (b11_ptr, stat=err(1,2))
    deallocate (b21_ptr, stat=err(2,2))
    deallocate (b31_ptr, stat=err(3,2))


    !! try the unlimited poly pointers assigned to targets that are not created
    !by allocate
    x1 => b2(10)
    x2 => b2(1:10:3)
    x3 => b3(1,::2)

    deallocate (x1, stat=err(1, 3))
    deallocate (x2, stat=err(2, 3))
    deallocate (x3, stat=err(3, 3))


    if (any (err /= 2)) error stop 1_4
end
