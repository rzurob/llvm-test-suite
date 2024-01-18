! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr037a1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/07/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (poly-pointer component
!                               in structure constructor; mix with select type)
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

    type point(k1)    ! (4)
        integer, kind :: k1
        real(k1)      :: x, y

        contains

        procedure, pass :: validate => validatePoint
    end type

    type, extends(point) :: point3D(k2)    ! (4,4)
        integer, kind :: k2
        real(k2)      :: z

        contains

        procedure, pass :: validate => validatePoint3D
    end type

    type, extends (point3D) :: colorPoint3D(k3)    ! (4,4,1)
        integer, kind :: k3
        integer(k3)   :: color = 1

        contains

        procedure, pass :: validate => validatecolorPoint3D
    end type

    contains

    logical function validatePoint (p, p1)
        class (point(4)), intent(in) :: p, p1

        validatePoint = (precision_r4 (p%x, p1%x) .and. &
                         precision_r4 (p%y, p1%y))
    end function

    logical function validatePoint3D (p, p1)
        class (point3D(4,4)), intent(in) :: p
        class (point(4)), intent(in) :: p1

        validatePoint3D = validatePoint (p, p1)

        !! need support of select type construct
        select type (p1)
            class is (point3D(4,4))
                validatePoint3D = validatePoint3D .and. &
                    (precision_r4(p%z, p1%z))
            class default
                error stop 10_4
        end select
    end function

    logical function validatecolorPoint3D (p, p1)
        class (colorPoint3D(4,4,1)), intent(in) :: p
        class (point(4)), intent(in) :: p1

        validatecolorPoint3D = validatePoint3D(p, p1)

        !! need support of select type construct
        select type (p1)
            type is (colorPoint3D(4,4,1))
                validatecolorPoint3D = validatecolorPoint3D .and. &
                    (p%color == p1%color)
            class default
                error stop 20_4
        end select
    end function
end module

module m1
use m

    type p2DPointer(k4)    ! (4)
        integer, kind             :: k4
        class(point(k4)), pointer :: p2D => null()
    end type

    type p3DPointer(k5)    ! (4)
        integer, kind                  :: k5
        class(point3D(k5,k5)), pointer :: p3D => null()
    end type

    type (p2DPointer(4)), allocatable :: pc1
    type (p3DPointer(4)), allocatable :: pc2
end module

program fconstr037a1
use m1
    class (point(4)), target, allocatable :: p1
    class (point3D(4,4)), pointer :: p3d1

    allocate(p1, source=point3D(4,4)(2.1, 1.3, 0.4))

    allocate (p3d1, source=colorPoint3D(4,4,1) (3.2, 4.5, -1.4, color=10))

    allocate (pc1, pc2)

    pc1 = p2DPointer(4) (p1)

    pc2 = p3DPointer(4) (p3d1)

    !! validate the components

    if (.not. pc1%p2D%validate (p1)) error stop 1_4

    if (.not. pc2%p3D%validate (colorPoint3D(4,4,1) (3.2, 4.5, -1.4, color=10))) &
            error stop 2_4
end
