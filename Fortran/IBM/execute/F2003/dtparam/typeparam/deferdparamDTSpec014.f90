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
!*  DATE                       : 01/13/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Deferred type-parameters: defined during
!                               intrinisic assignment; use pointer components.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child (n)
        integer, len :: n

        real(4) data(n)

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, 'empty type'
    end subroutine

    subroutine printChild (b)
        class (child(*)), intent(in) :: b

        print *, 'child type of length = ', b%n

        write (*, '(7f10.2)') b%data
    end subroutine
end module

module m1
use m
    type container
        class(base), pointer :: data(:) => null()
    end type
end module

program deferdparamDTSpec014
use m1
    type(container), allocatable :: co1(:)

    allocate (co1(100))

    !! assign values to element 10-30 
    do i = 10, 30
        if (mod (i, 2) == 0) then
            allocate (co1(i)%data(1+i/10))
        else
!            allocate (co1(i)%data(i/10+1), source= &
!                (/(child(i/5)((/(j*1.0e1+k, k=1, i/5)/)), j=1, i/10+1)/))
            allocate (child(i/5) :: co1(i)%data(i/10+1))

            select type (x => co1(i)%data)
                type is (child(*))
                    do j = 1, i/10+1
                        x(j)%data = (/(j*1.0e1+k, k=1, i/5)/)
                    end do
            end select
        end if
    end do


    !! do scalar assignment
    co1(90) = co1(10)

    !! do array assignment
    co1(70:89) = co1(30:11:-1)

    !! verify
    do i = 10, 30
        if (mod(i, 2) == 0) then
            if (.not. associated(co1(100-i)%data)) error stop 1_4
        else
            if (.not. associated(co1(100-i)%data, co1(i)%data)) error stop 2_4
        end if
    end do

    do i = 50, 100
        if (.not. associated(co1(i)%data)) cycle

        do j = 1, 1+(100-i)/10
            call co1(i)%data(j)%print
        end do
    end do
end
