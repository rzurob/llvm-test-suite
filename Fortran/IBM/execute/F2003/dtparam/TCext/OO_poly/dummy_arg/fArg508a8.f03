! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg508a8.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (INTENT(OUT) for
!                               explicit-shape unlimited poly dummy arg array;
!                               associated actual-args are unlimited poly array
!                               sections)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id = -1

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    class (*), allocatable :: x1_m(:)

    type (child(4,1,20)), target, save :: c1_m (3:5)

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine reset (b)
        class (*), intent(out) :: b(2)
    end subroutine
end module


program fArg508a8
use m
    type (base(4)), target :: b1(3)

    class (*), pointer :: b_ptr(:)

    b1%id = (/1,2,3/)

    c1_m%id = (/3,4,5/)
    c1_m%name = (/'c1_m_3', 'c1_m_4', 'c1_m_5'/)

    b_ptr => b1

    call reset (b_ptr(::2))     !<-- b1(1), (3)

    b_ptr => c1_m(::2)

    allocate (x1_m(3), source=child(4,1,20)(200,'x1_m'))

    call reset (b_ptr)          !<-- c1_m(3), (5)
    call reset (x1_m(2:))        !<-- x1_m(2), (3)

    do i = 1, 3
        call b1(i)%print

        call c1_m(i+2)%print

        select type (x => x1_m(i))
            class is (base(4))
                call x%print
            class default
                error stop 1_4
        end select
    end do

    deallocate (x1_m)
end
