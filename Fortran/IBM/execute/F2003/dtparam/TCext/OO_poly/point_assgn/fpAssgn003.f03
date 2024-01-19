! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn003.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : pointer assignment (non-polymorphic pointer
!*                               assigned to polymorphic target)
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
    type point(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        real(k1)      :: x, y

        contains

        procedure, pass :: print => printPoint
    end type

    type, extends(point) :: point3D    ! (20,4)
        real(k1) :: z

        contains

        procedure, pass :: print => printPoint3D
    end type

    type, extends(point3D) :: colorPoint3D(k2)    ! (20,4,1)
        integer, kind :: k2
        integer(k2)   :: color = 0

        contains

        procedure :: print => printColorPoint3D
    end type

    contains

    subroutine printPoint(p)
        class (point(*,4)), intent(in) :: p

        print *, p%x, ', ', p%y
    end subroutine

    subroutine printPoint3D (p)
        class(point3D(*,4)), intent(in) :: p

        print *, 'point3D'
        call p%point%print
        print *, p%z
    end subroutine

    subroutine printColorPoint3D (p)
        class(colorPoint3D(*,4,1)), intent(in) :: p

        print *, 'colorPoint3D'
        print *, p%x, ',', p%y, ',', p%z, 'color = ', p%color
    end subroutine
end module

program fpAssgn003
use m
    type (colorPoint3D(20,4,1)), target :: pc3d = colorPoint3D(20,4,1)(0.0, 1.0, 1.0, 1)

    class (point(:,4)), pointer :: p2d_ptr1
    class (point3D(20,4)), pointer :: p3d_ptr, p3d_ptr1
    class (colorPoint3D(20,4,1)), pointer :: pc3d_ptr

    !! this is the LHS
    type (point(20,4)), pointer :: p2d_ptr2
    type (point3D(20,4)), pointer :: p3d_ptr2

    p2d_ptr1 => pc3d

    p2d_ptr2 => p2d_ptr1    !p2d_ptr2 associated with pc3d%point

    if (.not. associated (p2d_ptr2, pc3d%point)) error stop 1_4
    call p2d_ptr2%print

    allocate (p3d_ptr, pc3d_ptr)

    p3d_ptr%x = 1
    p3d_ptr%y = 1
    p3d_ptr%z = 1

    p2d_ptr1 => p3d_ptr
    p2d_ptr2 => p2d_ptr1  !p2d_ptr2 associated with p3d_ptr%point

    if (.not. associated (p2d_ptr2, p3d_ptr%point)) error stop 2_4

    call p2d_ptr2%print

    pc3d_ptr%x = 1
    pc3d_ptr%y = 1
    pc3d_ptr%z = 1
    pc3d_ptr%color = 1


    p2d_ptr1 => pc3d_ptr

    p2d_ptr2 => p2d_ptr1 !p2d_ptr2 associated with pc3d_ptr%point

    if (.not. associated (p2d_ptr2, pc3d_ptr%point)) error stop 3_4

    call p2d_ptr2%print

    p2d_ptr1 => pc3d_ptr
    p2d_ptr2 => p2d_ptr1 !p2d_ptr2 associated with pc3d_ptr%point

    if (.not. associated (p2d_ptr2, pc3d_ptr%point)) error stop 4_4

    call p2d_ptr2%print

    p3d_ptr1 => pc3d_ptr
    p3d_ptr2 => p3d_ptr1 !p3d_ptr2 associated with pc3d_ptr%point3D

    if (.not. associated (p3d_ptr2, pc3d_ptr%point3D)) error stop 5_4

    call p3d_ptr2%print

    p3d_ptr2 => p3d_ptr ! same declared and dynamic types

    if (.not. associated (p3d_ptr2, p3d_ptr)) error stop 6_4

    call p3d_ptr2%print

    deallocate (p3d_ptr, pc3d_ptr)
end
