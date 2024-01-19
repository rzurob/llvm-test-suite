! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/07/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (assumed-shape array
!                               associated with assumed-shape array)
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

    type, extends(base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine test1 (b)
        class (base), intent(in) :: b (3:)

        do i = 3, size(b)+2
            call b(i)%print
        end do

        call test2 (b)
    end subroutine

    subroutine test2 (b)
        type (base), intent(in) :: b (:)

        do i = 1, size (b)
            call b(i)%print
        end do
    end subroutine
end module

program fArg027a1
use m
    type (child) :: c1 (3)

    class (base), pointer :: b1 (:)

    c1 = (/child(1,'c1_1'), child(2,'c1_2'), child(3,'c1_3')/)

    call test1 (c1)

    allocate (b1(3), source=c1(3:1:-1))

    call test1 (b1)

    deallocate (b1)
end
