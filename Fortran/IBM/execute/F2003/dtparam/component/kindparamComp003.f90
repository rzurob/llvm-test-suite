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
!*  DESCRIPTION                : dtparam (section 4.5.3: components)
!                               Case: Kind type parameter and component:
!                               poly-allocatable parameterized component; array
!                               components.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point (k)
        integer, kind :: k

        real(k) :: x, y
    end type

    type, extends(point) :: point3D
        real(k) :: z
    end type

    contains

    subroutine printPoint4 (p)
        class(point(4)), intent(in) :: p

        select type (p)
            type is (point(4))
                write (*, '(2f10.2)') p%x, p%y

            type is (point3D(4))
                write (*, '(3f10.2)') p%x, p%y, p%z

            class default
                stop 1
        end select
    end subroutine

    subroutine printPoint8 (p)
        class(point(8)), intent(in) :: p

        select type (p)
            type is (point(8))
                write (*, '(2f15.5)') p%x, p%y

            type is (point3D(8))
                write (*, '(3f15.5)') p%x, p%y, p%z

            class default
                stop 10
        end select
    end subroutine
end module

module n
use m
    type curve (k)
        integer, kind :: k

        class (point(k)), allocatable :: p1(:)
    end type
end module

program kindparamComp003
use n
    type(curve(4)) c1(10)
    type(curve(8)), pointer :: c2(:)

    allocate (c2(10))

    allocate (c1(1)%p1(0:2), source=(/point3D(4)(1.0, 2.0, 3.0), &
            point3D(4)(3.0, 4.0, 5.0), point3D(4)(5.0, 6.0, 7.0)/))

    allocate (c2(2)%p1(-1:0), source=(/point(8)(1.2d0, 2.2d0), &
            point(8)(-1.2d0,-2.2d0)/))

    c1(9:10) = c1(1)

    c2(5) = c2(2)

    !! verify
    if ((.not. allocated(c1(9)%p1)) .or. (.not. allocated(c1(10)%p1)) .or. &
        (.not. allocated(c2(5)%p1))) error stop 1_4

    if ((lbound(c1(9)%p1,1) /= 0) .or. (ubound(c1(9)%p1,1) /= 2)) error stop 2_4
    if ((lbound(c1(10)%p1,1) /= 0) .or. (ubound(c1(10)%p1,1)/=2)) error stop 3_4

    if ((lbound(c2(5)%p1,1) /= -1) .or. (ubound(c2(5)%p1,1) /=0)) error stop 4_4

    do i = 0, 2
        call printPoint4(c1(9)%p1(i))
        call printPoint4(c1(10)%p1(i))
    end do

    do i = -1, 0
        call printPoint8(c2(5)%p1(i))
    end do
end
