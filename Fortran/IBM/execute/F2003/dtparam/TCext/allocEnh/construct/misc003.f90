! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=none /tstdev/F2003/allocEnh/construct/misc003.f
! opt variations: -qnol -qnodeferredlp -qreuse=base

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/20/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               A recursion which involves select type
!                               construct.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point(n1,k1)    ! (20,8)
        integer, kind         :: k1
        integer, len          :: n1
        real(k1), allocatable :: x, y

        contains

        procedure :: half => halfPointVal
    end type

    type, extends(point) :: point3D(n2,k2)    ! (20,8,20,8)
        integer, kind         :: k2
        integer, len          :: n2
        real(k2), allocatable :: z

        contains

        procedure :: half => halfPoint3DVal
    end type

    contains

    function halfPointVal (p)
        class(point(*,8)), intent(in) :: p

        class(point(:,8)), pointer :: halfPointVal

        allocate(point(20,8) :: halfPointVal)

        if (allocated(p%x)) halfPointVal%x = p%x/2.0d0
        if (allocated(p%y)) halfPointVal%y = p%y/2.0d0
    end function

    function halfPoint3DVal (p)
        class(point3D(*,8,*,8)), intent(in) :: p

        class(point(:,8)), pointer :: halfPoint3DVal

        allocate(halfPoint3DVal, source=p)

        select type (x => halfPoint3DVal)
            class is (point3D(*,8,*,8))
                if (allocated(x%x)) x%x = x%x /2.0d0
                if (allocated(x%y)) x%y = x%y /2.0d0
                if (allocated(x%z)) x%z = x%z /2.0d0

            class default
                stop 10
        end select
    end function

    recursive subroutine approachZero (p, howclose)
        class (point(:,8)), intent(in), pointer :: p
        real(8), optional :: howclose

        class(point(:,8)), pointer :: temp

        real(8) :: limit = 1.0d-14
        logical needHalf

        integer :: count = 0

        if (present(howclose)) limit = howclose


        count = count + 1

        needHalf = .false.

        if (allocated(p%x)) needHalf = needHalf .or. (abs(p%x) > limit)
        if (allocated(p%y)) needHalf = needHalf .or. (abs(p%y) > limit)

        select type (p)
            type is (point(*,8))
            class is (point3D(*,8,*,8))
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
    class(point(:,8)), pointer :: p1

    class(point(:,8)), allocatable :: p2

    allocate(p1, source=point3D(20,8,20,8)(2.0_8, 10.5d0, -1.2d0))

    call approachZero (p1, 1.0d-14)

    allocate (point(20,8) :: p2)

    select type (x => p2)
        type is (point(*,8))
            x = point(20,8)(1.2d2, 5.5d0)

            call approachZero (x%half(), 5.0d-10)

        class default
            stop 5
    end select
end
