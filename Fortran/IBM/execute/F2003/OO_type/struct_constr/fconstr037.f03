! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (nonpolymorphic pointer
!*                               component initilized by polymorphic pointer)
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
    logical, external :: precision_r4

    type point
        real*4 :: x, y
    end type

    type, extends(point) :: point3D
        real*4 :: z
    end type

    type, extends (point3D) :: colorPoint3D
        integer*1 :: color = 1
    end type

    contains

    logical function validatePoint (p, p1)
        type (point), intent(in) :: p, p1

        validatePoint = (precision_r4 (p%x, p1%x) .and. &
                         precision_r4 (p%y, p1%y))
    end function

    logical function validatePoint3D (p, p1)
        type (point3D), intent(in) :: p, p1

        validatePoint3D = (precision_r4 (p%z, p1%z) .and. &
                           validatePoint (p%point, p1%point))
    end function

    logical function validatecolorPoint3D (p, p1)
        type (colorPoint3D), intent(in) :: p, p1

        validatecolorPoint3D = ((p%color == p1%color) .and. &
                validatePoint3D(p%point3D, p1%point3D))
    end function
end module

module m1
use m

    type p2DPointer
        type (point), pointer :: p2D => null()
    end type

    type p3DPointer
        type (point3D), pointer :: p3D => null()
    end type
end module

program fconstr037
use m1

    type (p2DPointer) :: p2d_P1
    type (p3DPointer) :: p3d_P1

    type (point), target :: p2d1 = point (0.0, 0.0)
    type (point3D), target :: p3d1 = point3D(z=1.0, point=point(x=1.0, y=1.0))
    type (colorPoint3D), target :: pc3d1 = &
        colorPoint3D (point3D=point3D(0.0, 1.0, 0.0), color=2)


    class(point), pointer :: p2d_ptr => null()
    class (point3D), pointer :: p3d_ptr => null()

    !! validate all the target objects
    if (.not. validatePoint (p2d1, point (y=0.0, x=0.0))) error stop 15_4

    if (.not. validatePoint3D (p3d1, point3D (point=point(1.0, 1.0), z=1.0))) &
            error stop 16_4

    if (.not. validateColorPoint3D (pc3d1, colorPoint3D(point3D=point3D(point= &
            point(x=0.0, y=1.0), z=0.0), color=2))) error stop 17_4



    p2d_P1 = p2DPointer (p2D = p2d_ptr)
    p3d_P1 = p3DPointer (p3D = p3d_ptr)

    if (associated(p2d_P1%p2D) .or. associated(p3d_P1%p3D)) error stop 1_4

    !! assign the poly pointers and use them for structure constructors
    p2d_ptr => pc3d1
    p3d_ptr => pc3d1

    p2d_P1 = p2DPointer (p2D = p2d_ptr)
    p3d_P1 = p3DPointer (p3D = p3d_ptr)

    if (.not. associated (p2d_P1%p2D, pc3d1%point)) error stop 2_4

    if (.not. validatePoint (p2d_P1%p2D, point(0.0, 1.0))) error stop 3_4

    if (.not. associated (p3d_P1%p3D, pc3d1%point3D)) error stop 4_4

    if (.not. validatePoint3D (p3d_P1%p3D, pc3d1%point3D)) error stop 5_4

    p3d_ptr => p3d1
    p2d_ptr => p3d_ptr

    p2d_P1 = p2DPointer (p2d_ptr)

    if (.not. associated (p2d_P1%p2D, p3d1%point)) error stop 6_4

    if (.not. validatePoint (p2d_P1%p2D, point(1.0, 1.0))) error stop 7_4
end