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
!*  DATE                       : 01/17/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 315110)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point! (k)
        real(4) :: x, y
    end type

    type, extends(point) :: point3D
        real(4) :: z
    end type

    contains

    subroutine printPoint (p)
        class(point), intent(in) :: p

        select type (p)
            type is (point)
                write (*, '(2f10.2)') p%x, p%y

            type is (point3D)
                write (*, '(3f15.5)') p%x, p%y, p%z

            class default
                stop 1
        end select
    end subroutine
end module

module n
use m
    type A
        class (point), allocatable :: p1(:)
    end type
end module

program kindparamComp003
use n
    type(A) c1(10)

    allocate (c1(1)%p1(0:2), source=(/point3D(1.0, 2.0, 3.0), &
            point3D(3.0, 4.0, 5.0), point3D(5.0, 6.0, 7.0)/))

    c1(9:10) = c1(1)

    c1(5) = c1(1)
    c1(6) = c1(1)

    if ((.not. allocated(c1(9)%p1)) .or. (.not. allocated(c1(10)%p1))) &
                error stop 1_4

    if ((.not. allocated(c1(5)%p1)) .or. (.not. allocated(c1(6)%p1))) &
                error stop 2_4

    do i = 0, 2
        call printPoint(c1(6)%p1(i))
        call printPoint(c1(9)%p1(i))
        call printPoint(c1(10)%p1(i))
    end do
end
