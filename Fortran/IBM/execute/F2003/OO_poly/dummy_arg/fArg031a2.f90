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
!*  DATE                       : 01/28/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : argument association (dummy-arg as the
!                               actual-arg; test sequence association)
!*
!*  KEYWORD(S)                 :
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

    subroutine printChild (c)
        class (child), intent(in) :: c

        print *, c%id, c%name
    end subroutine

    subroutine printBase (c)
        class (base), intent(in) :: c

        print *, c%id
    end subroutine

    subroutine test1 (b)
        class (base), intent(inout) :: b(:)

        call test2 (b%id)

        call test2 (b(::2))
    end subroutine

    subroutine test2 (x)
        class (*), intent(inout) :: x(2:4)

        select type (x)
            type is (integer(4))
                print *, x
            class is (base)
                do i = 2, 4
                    call x(i)%print
                end do
            class default
                print *, 'unknown type'
        end select
    end subroutine
end module

program fArg031a2
use m
    class (base), allocatable :: b1 (:)
    type (child) :: c1 (10)


    allocate (b1(10), source=(/(child(i,'c1_'//char(ichar('0')+i-1)), i=1,10)/))

    call test1 (b1)

    call test1 (b1(1:9:2))
end
