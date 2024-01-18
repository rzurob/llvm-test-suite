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
!*  DATE                       : 09/20/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               A recursion which involves select type
!                               construct.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point
        real(8), allocatable :: x, y

        contains

        procedure :: half => halfPointVal
    end type

    type, extends(point) :: point3D
        real(8), allocatable :: z

        contains

        procedure :: half => halfPoint3DVal
    end type

    contains

    function halfPointVal (p)
        class(point), intent(in) :: p

        class(point), pointer :: halfPointVal

        allocate(halfPointVal)

        if (allocated(p%x)) halfPointVal%x = p%x/2.0d0
        if (allocated(p%y)) halfPointVal%y = p%y/2.0d0
    end function

    function halfPoint3DVal (p)
        class(point3D), intent(in) :: p

        class(point), pointer :: halfPoint3DVal

        allocate(halfPoint3DVal, source=p)

        select type (x => halfPoint3DVal)
            class is (point3D)
                if (allocated(x%x)) x%x = x%x /2.0d0
                if (allocated(x%y)) x%y = x%y /2.0d0
                if (allocated(x%z)) x%z = x%z /2.0d0

            class default
                stop 10
        end select
    end function

    recursive subroutine approachZero (p, howclose)
        class (point), intent(in), pointer :: p
        real(8), optional :: howclose

        class(point), pointer :: temp

        real(8) :: limit = 1.0d-14
        logical needHalf

        integer :: count = 0

        if (present(howclose)) limit = howclose


        count = count + 1

        needHalf = .false.

        if (allocated(p%x)) needHalf = needHalf .or. (abs(p%x) > limit)
        if (allocated(p%y)) needHalf = needHalf .or. (abs(p%y) > limit)

        select type (p)
            type is (point)
            class is (point3D)
                if (allocated(p%z)) needHalf = needHalf .or. (abs(p%z) > limit)

            class default
                stop 20
        end select

        if (needHalf) then
            temp => p%half()

            call approachZero (temp)

            deallocate(temp)
        else
            print *, count
            count = 0
        end if


    end subroutine
end module

use m
    class(point), pointer :: p1

    class(point), allocatable :: p2

    allocate(p1, source=point3D(2.0_8, 10.5d0, -1.2d0))

    call approachZero (p1, 1.0d-14)

    allocate (p2)

    select type (x => p2)
        type is (point)
            x = point(1.2d2, 5.5d0)

            call approachZero (x%half(), 5.0d-10)

        class default
            stop 5
    end select
end
