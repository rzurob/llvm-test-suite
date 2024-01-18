!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg021a_2.f
! %VERIFY: fArg021a_2.out:fArg021a_2.vf
! %STDIN:
! %STDOUT: fArg021a_2.out
! %EXECARGS:
! %POSTCMD:
! %END
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
    type base
        integer*4 :: id
    end type

    type, extends (base) :: child
        character*20 :: name
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
            type is (base)
                print *, x
            type is (child)
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
    type (base) :: b_const
    type (child) :: c_const

    parameter (b_const = base(10), c_const = child (1, 'xlf test 101'))

    type (child), parameter :: c_const_array (3) = (/child (100, 'c_1'), &
        child (200, 'c_2'), child(300, 'c_3')/)


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
