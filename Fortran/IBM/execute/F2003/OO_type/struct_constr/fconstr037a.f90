!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr037a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/07/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (non-poly pointer
!                               component points to poly-pointer data source)
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

        contains

        procedure, pass :: validate => validatePoint
    end type

    type, extends(point) :: point3D
        real*4 :: z

        contains

        procedure, pass :: validate => validatePoint3D
    end type

    type, extends (point3D) :: colorPoint3D
        integer*1 :: color = 1

        contains

        procedure, pass :: validate => validatecolorPoint3D
    end type

    contains

    logical function validatePoint (p, p1)
        class (point), intent(in) :: p, p1

        validatePoint = (precision_r4 (p%x, p1%x) .and. &
                         precision_r4 (p%y, p1%y))
    end function

    logical function validatePoint3D (p, p1)
        class (point3D), intent(in) :: p
        class (point), intent(in) :: p1

        validatePoint3D = validatePoint (p, p1)

        !! need support of select type construct
        select type (p1)
            type is (point3D)
                validatePoint3D = validatePoint3D .and. &
                    (precision_r4(p%z, p1%z))
            class default
                error stop 10_4
        end select
    end function

    logical function validatecolorPoint3D (p, p1)
        class (colorPoint3D), intent(in) :: p
        class (point), intent(in) :: p1

        validatecolorPoint3D = validatePoint3D(p, p1)

        !! need support of select type construct
        select type (p1)
            type is (colorPoint3D)
                validatecolorPoint3D = validatecolorPoint3D .and. &
                    (p%color == p1%color)
            class default
                error stop 20_4
        end select
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

program fconstr037a
use m1

    type (p2DPointer) :: p2d_P1
    type (p3DPointer) :: p3d_P1

    type (point), target :: p2d1 = point (0.0, 0.0)
    type (point3D), target :: p3d1 = point3D(z=1.0, point=point(x=1.0, y=1.0))
    type (colorPoint3D), target :: pc3d1 = &
        colorPoint3D (point3D=point3D(0.0, 1.0, 0.0), color=2)


    class(point), pointer :: p2d_ptr => null()
    class (point3D), pointer :: p3d_ptr => null()

    !! use null pointers
    p2d_P1 = p2DPointer (p2D = p2d_ptr)
    p3d_P1 = p3DPointer (p3D = p3d_ptr)

    if (associated(p2d_P1%p2D) .or. associated(p3d_P1%p3D)) error stop 1_4

    !! assign the poly pointers and use them for structure constructors
    p2d_ptr => pc3d1
    p3d_ptr => pc3d1

    p2d_P1 = p2DPointer (p2D = p2d_ptr)
    p3d_P1 = p3DPointer (p3D = p3d_ptr)

    if (.not. associated (p2d_P1%p2D, pc3d1%point)) error stop 2_4

    if (.not. p2d_P1%p2D%validate (point (0.0,1.0))) error stop 3_4
    if (.not. p3d_P1%p3D%validate (point3D(0.0, 1.0, 0.0))) error stop 4_4
end
