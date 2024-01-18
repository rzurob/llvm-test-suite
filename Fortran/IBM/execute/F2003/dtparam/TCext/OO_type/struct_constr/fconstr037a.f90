! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr037a.f
!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
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
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 12/07/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
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
            type is (point3D(4,4))
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
        integer, kind            :: k4
        type(point(k4)), pointer :: p2D => null()
    end type

    type p3DPointer(k5)    ! (4)
        integer, kind                 :: k5
        type(point3D(k5,k5)), pointer :: p3D => null()
    end type
end module

program fconstr037a
use m1

    type (p2DPointer(4)) :: p2d_P1
    type (p3DPointer(4)) :: p3d_P1

    type (point(4)), target :: p2d1 = point(4) (0.0, 0.0)
    type (point3D(4,4)), target :: p3d1 = point3D(4,4)(z=1.0, point=point(4)(x=1.0, y=1.0))
    type (colorPoint3D(4,4,1)), target :: pc3d1 = &
        colorPoint3D(4,4,1) (point3D=point3D(4,4)(0.0, 1.0, 0.0), color=2)


    class(point(4)), pointer :: p2d_ptr => null()
    class (point3D(4,4)), pointer :: p3d_ptr => null()

    !! use null pointers
    p2d_P1 = p2DPointer(4) (p2D = p2d_ptr)
    p3d_P1 = p3DPointer(4) (p3D = p3d_ptr)

    if (associated(p2d_P1%p2D) .or. associated(p3d_P1%p3D)) error stop 1_4

    !! assign the poly pointers and use them for structure constructors
    p2d_ptr => pc3d1
    p3d_ptr => pc3d1

    p2d_P1 = p2DPointer(4) (p2D = p2d_ptr)
    p3d_P1 = p3DPointer(4) (p3D = p3d_ptr)

    if (.not. associated (p2d_P1%p2D, pc3d1%point)) error stop 2_4

    if (.not. p2d_P1%p2D%validate (point(4) (0.0,1.0))) error stop 3_4
    if (.not. p3d_P1%p3D%validate (point3D(4,4)(0.0, 1.0, 0.0))) error stop 4_4
end
