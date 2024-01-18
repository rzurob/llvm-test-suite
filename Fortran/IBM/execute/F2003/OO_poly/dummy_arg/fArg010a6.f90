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
!*  DATE                       : 05/18/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : argument association (VALUE attribute)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    contains

    subroutine testInt1 (x)
        integer, value :: x

        x = x + 100
        call print1(x)
    end subroutine

    subroutine testCmplx1 (x)
        complex, value :: x

        x = x + (10.0, 10.0)
        call print1 (x)
    end subroutine

    subroutine testChar1 (x)
        character(5), value :: x

        x = 'value changed'
        call print1 (x)
    end subroutine

    subroutine test1 (x)
        class(*) :: x

        select type (x)
            type is (integer)
                call testInt1 (x)

            type is (complex)
                call testCmplx1 (x)

            type is (character(*))
                call testChar1 (x)

            class default
                error stop 10_4
        end select
    end subroutine

    subroutine print1 (x)
        class(*) :: x

        select type (x)
            type is (integer)
                print *, x
            type is (complex)
                write (*, '(2f12.2)') x
            type is (character(*))
                print *, x
            class default
                error stop 11_4
        end select
    end subroutine
end module

program fArg010a6
use m
    class (*), pointer :: x1(:)

    !! test 1
    allocate (x1(2), source=(/2, 6/))

    call test1(x1(2))
    call print1 (x1(2))

    deallocate (x1)

    !! test 2
    allocate (x1(10), source='abcde')

    call test1(x1(8))
    call print1 ((x1(8)))

    !! test 3

    deallocate (x1)

    allocate (x1(1), source=(10.2, 3.1))

    call test1(x1(1))
    call print1(x1(1))
end
