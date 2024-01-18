!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 05/03/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : poly function return (defined operator)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(8), allocatable :: r1

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character(20) :: name

        contains

        procedure :: print => printChild
    end type

    interface operator(-)
        module procedure minus
    end interface

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        if (allocated(b%r1)) then
            write (*, '(f10.2)') b%r1
        else
            print *, 'r1 not allocated'
        end if
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        if (allocated(b%r1)) then
            write (*, '(f10.2, 2a)') b%r1, ', ', b%name
        else
            print *, '|, ', b%name
        end if
    end subroutine

    class(base) function minus (b)
        class (base), intent(in) :: b
        allocatable minus

        select type (b)
            type is (base)
                if (allocated (b%r1)) then
                    allocate (minus, source=base(-b%r1))
                else
                    allocate (minus)
                end if
            type is (child)
                if (allocated (b%r1)) then
                    allocate (minus, source=child(-b%r1, '-'//b%name))
                else
                    allocate (minus, source=child(null(), '-'//b%name))
                end if
            class default
        end select
    end function
end module

program ffuncRet009
use m
    class (base), pointer :: b1

    type(base) :: b2

    allocate (b1)
    allocate (b1%r1, source= 2.5_8+3.1_8)

    associate (x1 => (- b1), x2 => -child(1.5, 'test 01'))
        call x1%print
        call x2%print
    end associate

    !! 2nd test
    deallocate (b1)

    allocate (b1, source=-child(-3.2, 'test 02'))

    call b1%print

    deallocate (b1%r1)

    b2 = -b1

    call b1%print
    call b2%print
end
