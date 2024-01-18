! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/F2003/allocEnh/construct/definedAssgn008.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/31/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               More test on that the defined assignment is
!                               invoked at extended type level; the derived type
!                               with unlimited poly-allocatable component.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        real(k1)         x(2)

        contains

        procedure :: assgnPoint
        generic :: assignment(=) => assgnPoint
    end type

    type, extends(point) :: uPoint(k2,n2)    ! (20,4,4,20)
        integer, kind :: k2
        integer, len  :: n2
        contains

        procedure :: assgnPoint => assgnUpoint
        generic :: assignment(=) => assgnPoint
    end type

    type line(k3,n3)    ! (4,20)
        integer, kind                   :: k3
        integer, len                    :: n3
        class(point(:,k3)), allocatable :: points(:)
    end type

    contains

    elemental subroutine assgnPoint (u1, u2)
        class(point(*,4)), intent(out) :: u1
        class(point(*,4)), intent(in) :: u2

        u1%x = u2%x + 1.0
    end subroutine

    elemental subroutine assgnUpoint (u1, u2)
        class(uPoint(*,4,4,*)), intent(out) :: u1
        class(point(*,4)), intent(in) :: u2

        real radius

        radius = sqrt(u2%x(1)**2 + u2%x(2)**2)

        if (radius < 1.0e-5) then
            u1%x = sqrt(-1.0)
        else
            u1%x = u2%x/radius
        end if
    end subroutine
end module

module m1
    type container(k4,n4)    ! (4,20)
        integer, kind :: k4
        integer, len  :: n4
        class(*), allocatable :: data
    end type
end module

program definedAssgn008
use m
use m1
    type(container(4,:)), allocatable :: co1
    type(container(4,20)) co2

    logical(4), external :: precision_r4

    allocate (co2%data, source=line(4,20)([uPoint(20,4,4,20)([1.0, 2.0]), uPoint(20,4,4,20)([3.0,4.0])]))

    co1 = co2

    select type (x => co1%data)
        class is (line(4,*))
            select type (y => x%points)
                class is (point(*,4))
                    if (.not. precision_r4(y(1)%x(1), 0.4472135901_4)) &
                        error stop 1_4

                    if (.not. precision_r4(y(1)%x(2), 0.8944271803_4)) &
                        error stop 2_4

                    if (.not. precision_r4(y(2)%x(1), 0.6_4)) &
                        error stop 3_4

                    if (.not. precision_r4(y(2)%x(2), 0.8_4)) &
                        error stop 4_4

                class default
                    stop 20
            end select

        class default
            stop 10

    end select
end
