! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (array pointer
!*                               assignment; one dimensional; basic pass binding
!*                               tests)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point
        real*4 :: x, y

        contains

        procedure, pass :: print => printPoint
    end type

    type, extends(point) :: point3D
        real*4 :: z

        contains

        procedure, pass :: print => printPoint3D
    end type

    contains

    subroutine printPoint (p)
        class (point), intent(in) :: p

        print *, 'x = ', p%x, 'y = ', p%y
    end subroutine

    subroutine printPoint3D (p)
        class (point3D), intent(in) :: p

        call p%point%print
        print *, 'z = ', p%z
    end subroutine

end module

program fpAssgn001a3
use m
    type(point3D), target :: p3d(-3:6)
    type (point), pointer :: p1 (:)

    class (point), pointer :: p_ptr(:)

    p3d = (/(point3D(i,i,i), i=1,10)/)

    p_ptr => p3d

    if ((lbound(p_ptr,1) /= -3) .or. (ubound(p_ptr,1) /= 6)) error stop 1_4

    if (size(p_ptr) /= 10) error stop 2_4

    do i = 1, 10
        call p_ptr(i-4)%print
    end do

    allocate (p1(5))

    p1 = (/(point(i,i), i=1,5)/)

    p_ptr => p1

    if ((lbound(p_ptr,1) /= 1) .or. (ubound(p_ptr,1) /= 5)) error stop 3_4

    do i = 0, 4
        call p_ptr(i+1)%print
    end do

    deallocate (p1)
end
