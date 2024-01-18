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
!*  DATE                       : 01/09/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Kind type-parameter in
!                               declaration-type-spec: implict stmt.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k = 4

        integer(k) :: id = -1
        real(2*k) :: data = 1.0
    end type

    type, extends(base) :: child (n)
        integer, len :: n

        character(n) :: name = 'default'
    end type
end module

program kindparamImplct002
use m
    implicit class(base) (a-b), class(base(k=2)) (z)

    pointer b1(:)
    allocatable z1

    class(base(k=2)), allocatable :: a1

    allocate (child(4, n=20) :: b1(10))
    allocate (z1)
    allocate (a1, source=child(2, 15)(data=1.53e0, name='a1 by allocate'))

    call printBase2(z1)
    call printBase2(a1)

    do i = 1, 10
        call printBase4(b1(i))
    end do

    contains

    subroutine printBase2 (b)
        class(base(2)), intent(in) :: b

        select type (b)
            class is (base(2))
                write (*, '(i5, f10.2)') b%id, b%data

            class is (child(2, *))
                write (*, '(i5, f10.2, a,a)') b%id, b%data, '; ', b%name

            class default
                error stop 1_4
        end select
    end subroutine

    subroutine printBase4 (b)
        class(base(k=4)), intent(in) :: b

        select type (b)
            class is (base(4))
                write (*, '(i8, g15.5)') b%id, b%data

            class is (child(4, *))
                write (*, '(i8, g15.5, a, a)') b%id, b%data, '; ', b%name

            class default
                error stop 2_4

        end select
    end subroutine
end
