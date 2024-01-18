! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=none /tstdev/OO_tpbnd/specific/ftpbnd523.f
! opt variations: -qnol -qnodeferredlp -qreuse=base

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
!*  DATE                       : 03/02/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : specific type bound (use of select type in the
!                               type bound)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        real(k1)         x, y

        contains

        procedure :: distanceFrom => lengthBetween
    end type

    contains

    real function lengthBetween (p1, p2)
        class (point(*,4)), intent(in) :: p1, p2

        lengthBetween = (p1%x-p2%x)**2 + (p1%y - p2%y)**2

        lengthBetween = sqrt (lengthBetween)
    end function
end module

module m1
use m
    type, extends(point) :: point3D(n2,k2)    ! (20,4,20,4)
        integer, kind :: k2
        integer, len  :: n2
        real(k2)         z

        contains

        procedure :: distanceFrom => lengthBetweenPoint3D
    end type

    contains

    real function lengthBetweenPoint3D (p1, p2)
        class (point(*,4)), intent(in) :: p2
        class (point3D(*,4,*,4)), intent(in) :: p1

        select type (p2 => p2)
            class is (point3D(*,4,*,4))

            lengthBetweenPoint3D = (p1%x - p2%x)**2 + (p1%y - p2%y)**2 &
                                  +(p1%z - p2%z)**2

            lengthBetweenPoint3D = sqrt (lengthBetweenPoint3D)
            class default

            lengthBetweenPoint3D = -1.0
        end select
    end function
end module

program ftpbnd523
use m1
    class (point(:,4)), allocatable :: p1, p2, p3
    type (point(20,4)) :: p4
    logical precision_r4

    allocate (p1, source= point(20,4)(1.1, 2.1))

    allocate (p2, source= point3D(20,4,20,4)(2.2, 3.2, 4.3))
    allocate (p3, source=point3D(20,4,20,4) (3.4, 4.5, 5.6))

    p4 = p3

    if (.not. precision_r4(p2%distanceFrom (p1), -1.0)) error stop 1_4

    if (abs (p2%distanceFrom (p2)) > 1.0e-6) error stop 2_4

    if (.not. precision_r4 (p2%distanceFrom (p3), sqrt((2.2-3.4)**2 + &
                        (3.2-4.5)**2 + (4.3-5.6)**2))) error stop 3_4


    if (.not. precision_r4 (p1%distanceFrom (p4), sqrt((1.1-3.4)**2 + &
                        (2.1-4.5)**2))) error stop 4_4
end
