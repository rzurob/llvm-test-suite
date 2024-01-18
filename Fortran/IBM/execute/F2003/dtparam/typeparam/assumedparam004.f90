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
!*  DATE                       : 01/16/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Assumed type parameter in type-guard of
!                               select type.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program assumedparam004
    class (*), pointer :: x1(:)

    type base (n)
        integer, len :: n

        integer :: ids(n)
    end type

    allocate (base(100) :: x1(10))

    do i = 1, 10
        select type (y => x1(i))
            type is (base(*))
                if (size(y%ids) /= 100) error stop 2_4

                y%ids = (/(1000 * i + 2* j, j= 1, 100)/)
            class default
                error stop 1_4
        end select
    end do

    select type (x1)
        class is (base(*))
            if (x1%n /= 100) error stop 4_4

            do i = 1, 10
                if (size(x1(i)%ids) /= 100) error stop 5_4

                do j = 1, 100
                    if (x1(i)%ids(j) /= 1000 * i + 2* j) error stop 6_4
                end do
            end do
        class default
            error stop 3_4
    end select
end
