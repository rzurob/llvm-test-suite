! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr037a3.f
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
! %GROUP: fconstr037a3.f
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
!*  DATE                       : 12/08/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : structure constructor (poly-pointer components
!                               in structure during the intrinsic assignment;
!                               use FORALL construct)
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
                error stop 50_4
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
                error stop 70_4
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
end module

program fconstr037a
use m1
    type (p2DPointer(4)) :: pc1(10)
    type (p3DPointer(4)) :: pc2(10)


    !! set up the first 5 elements of pc1
    allocate (pc1(1)%p2D, source=point(4)(1.3, 2.5))
    allocate (pc1(2)%p2D, source=point3D(4,4)(1.3, 2.5, z=-1.2))
    allocate (pc1(3)%p2D, source=colorPoint3D(4,4,1)(point3D=point3D(4,4)(1.3, 2.5, &
                            z=-1.2), color=3))

    allocate (pc1(4)%p2D, source=point3D(4,4) (5.1, -2, 2.3))
    allocate (pc1(5)%p2D, source=pc1(3)%p2D)

    !! set up the last 5 element of pc2
    allocate (pc2(6)%p3D, source=point3D(4,4)(1.23, 3.435, 0))
    allocate (pc2(7)%p3D, source=colorPoint3D(4,4,1) (-1.1, 2.3, 0.2, 1))
    allocate (pc2(8)%p3D, source=pc2(7)%p3D)
    allocate (pc2(9)%p3D, source=point3D(4,4) (-1, -1, -1))
    allocate (pc2(10)%p3D, source=colorPoint3D(4,4,1)(.5,.5,-.5, 3))


    forall (i=1:5)
        pc1(11-i) = pc1(i)
        pc2(i) = pc2 (11-i)
    end forall


    !! verify that all the members are associated
    do i = 1, 5
        if (.not. associated (pc1(i)%p2D, pc1(11-i)%p2D)) error stop 1_4
        if (.not. associated (pc2(i)%p3D, pc2(11-i)%p3D)) error stop 2_4
    end do


    !! now use the type-bound to validate pc1
    if (.not. pc1(6)%p2D%validate(colorPoint3D(4,4,1)(1.3, 2.5, z=-1.2, color=3))) &
                error stop 3_4

    if (.not. pc1(7)%p2D%validate(point3D(4,4) (5.1, -2, 2.3))) error stop 4_4

    if (.not. pc1(8)%p2D%validate(colorPoint3D(4,4,1)(1.3, 2.5, z=-1.2, color=3))) &
                error stop 5_4

    if (.not. pc1(9)%p2D%validate(point3D(4,4)(1.3, 2.5, z=-1.2))) error stop 6_4


    if (.not. pc1(10)%p2D%validate(point(4)(1.3, 2.5))) error stop 7_4


    !! for pc2
    if (.not. pc2(1)%p3D%validate(colorPoint3D(4,4,1)(.5,.5,-.5, 3))) error stop 8_4

    if (.not. pc2(2)%p3D%validate(point3D(4,4) (-1, -1, -1))) error stop 9_4

    if (.not. pc2(3)%p3D%validate(colorPoint3D(4,4,1) (-1.1, 2.3, 0.2, 1))) &
                error stop 10_4

    if (.not. pc2(4)%p3D%validate(colorPoint3D(4,4,1) (-1.1, 2.3, 0.2, 1))) &
                error stop 11_4

    if (.not. pc2(5)%p3D%validate(point3D(4,4)(1.23, 3.435, 0))) error stop 12_4
end
