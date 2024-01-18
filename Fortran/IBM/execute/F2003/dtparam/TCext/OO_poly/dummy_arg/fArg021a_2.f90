! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg021a_2.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/01/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (named-const used as the
!                               actual-arg for unlimited poly dummy-arg; also
!                               tests that compiler will use temporaries that
!                               duplicate in value for the calls)
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
        integer(k1)   :: id
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    private abc
    contains

    subroutine test1 (y)
        class (*) :: y

        call abc (y)
    end subroutine

    subroutine abc (x)
        class(*), intent(inout) :: x

        select type (x)
            type is (base(4))
                print *, x
            type is (child(4,1,*))
                print *, x
            class default
                print *, 'other types'
        end select
    end subroutine

    subroutine test2 (y)
        class (*) y(:)

        do i = 1, size (y)
            print *, i
            call abc (y(i))
        end do
    end subroutine
end module

program fArg021a_2
use m
    type (base(4)) :: b_const
    type (child(4,1,20)) :: c_const

    parameter (b_const = base(4)(10), c_const = child(4,1,20) (1, 'xlf test 101'))

    type (child(4,1,20)), parameter :: c_const_array (3) = (/child(4,1,20) (100, 'c_1'), &
        child(4,1,20) (200, 'c_2'), child(4,1,20)(300, 'c_3')/)


    call test1 (b_const)
    call test1 (c_const)

    call test1 (c_const_array(2))

    call test1 (c_const_array(2)%name)
    call test1 (c_const_array(1)%base)
    call test1 (c_const%base)


    call test2 (c_const_array)
    call test2 (c_const_array%base)

    call test2 (c_const_array(::2))
    call test2 (c_const_array(::2)%name(2:5))

end
