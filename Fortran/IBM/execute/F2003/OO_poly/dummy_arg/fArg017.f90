!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg017.f
! %VERIFY: fArg017.out:fArg017.vf
! %STDIN:
! %STDOUT: fArg017.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (dummy-arg with TARGET
!                               attribute associated with actual-arg with no
!                               TARGET attribute)
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
        integer*4 :: id

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    class (base), allocatable :: b1_m
    class (base), pointer :: b_ptr, b_ptr2 (:)

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module


program fArg017
use m
    interface
        subroutine test1 (b)
        use m
            class (base), target, intent(inout) :: b
        end subroutine

        subroutine test2 (b)
        use m
            class (base), target, intent(inout) :: b(:)
        end subroutine
    end interface

    type (child) :: c1(2:4)

    allocate (b1_m, source=child(1, 'b1_m'))

    call test1 (b1_m)

    c1%name = (/'c1_2', 'c1_3','c1_4'/)

    call test2 (c1)

    do i = 2, 4
        call c1(i)%print
    end do
end

subroutine test1 (b)
use m
    class (base), target, intent(inout) :: b

    b_ptr => b

    call b_ptr%print

end subroutine

subroutine test2 (b)
use m
    class (base), target, intent(inout) :: b(:)

    b_ptr2 => b

    b_ptr2%id = -10
end subroutine
