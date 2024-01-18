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
!*  DATE                       : 04/21/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : structure constructor(rank-one non-poly pointer
!                               array component in structure constructor)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point
        real(4) :: x, y

        contains

        procedure, pass :: validate => validatePoint
    end type

    type, extends(point) :: point3D
        real*4 :: z

        contains

        procedure, pass :: validate => validatePoint3D
    end type

    type, extends (point3D) :: colorPoint3D
        integer(1) :: color = 1

        contains

        procedure, pass :: validate => validatecolorPoint3D
    end type

    contains

    pure logical function prec_r4 (r1, r2)
        real (4), intent(in) :: r1, r2

        prec_r4 = (2 * abs(r1 - r2) <= abs(r1 + r2) * 1.0e-5) .or. &
                  (abs(r1) <= 1.0e-15 .and. abs(r2) <= 1.0e-15)
    end function

    elemental logical function validatePoint (p, p1)
        class (point), intent(in) :: p, p1

        validatePoint = (prec_r4 (p%x, p1%x) .and. &
                         prec_r4 (p%y, p1%y))
    end function

    elemental logical function validatePoint3D (p, p1)
        class (point3D), intent(in) :: p
        class (point), intent(in) :: p1


        select type (p1)
            type is (point3D)
                validatePoint3D = p%point%validate(p1%point) .and. &
                    (prec_r4(p%z, p1%z))
            class default
                validatePoint3D = .false.
        end select
    end function

    elemental logical function validatecolorPoint3D (p, p1)
        class (colorPoint3D), intent(in) :: p
        class (point), intent(in) :: p1


        select type (p1)
            type is (colorPoint3D)
                validatecolorPoint3D = p%point3D%validate(p1%point3D) .and. &
                    (p%color == p1%color)
            class default
                validatecolorPoint3D = .false.
        end select
    end function
end module

module m1
use m

    type p2DPointer
        type (point), pointer :: p2D(:) => null()
    end type

    type p3DPointer
        type (point3D), pointer :: p3D(:) => null()
    end type

    type (p2DPointer), save :: pc1
end module

program fconstr037a2
use m1
    class(point), pointer :: p1(:)
    class (point3D), target, allocatable :: p3d1(:)

    type (colorPoint3D), target :: cp3d1(10)

    p1 => cp3d1(::2)

    cp3d1%color = (/(i, i=1,10)/)
    cp3d1%x = (/(j*1.0, j=1,10)/)
    cp3d1%y = cp3d1(10:1:-1)%x
    cp3d1%z = 0.0

    allocate (p3d1(10), source=cp3d1%point3D)

    associate (x => p3DPointer (cp3d1%point3D))
        if (.not. all (x%p3D%validate (p3d1))) error stop 1_4
    end associate

    associate (x => p2DPointer (p1))
        if (.not. all (x%p2D%validate (cp3d1(::2)%point))) error stop 2_4
    end associate
end
