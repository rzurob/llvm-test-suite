!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/19/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: A variant of dtparamExtends029: Explicit
!                               use of public in derived type defintion makes
!                               the derived type accessible.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    private     !<-- default to private

    type base
        private

        real(8), allocatable, public :: data
    end type

    type, public, extends(base) :: child (l)
        integer, len :: l

        private

        integer(8), public :: id = -1
        character(l), public :: name
    end type
end module

program dtparamExtends029a
use m
    type, extends(child) :: gen3 (n)
        integer, len :: n

        complex(8) cx(l: l+n-1)
    end type

    logical(4) precision_r8, precision_x6

    type (child(l=20)) c1
    type (gen3(15, n=2)) g1

    allocate(c1%data, source=1.12d1)
    c1%id = 2_8**34
    c1%name = 'c1 in main program'

    allocate (g1%data)

    g1%data = c1%data ** 3
    g1%id = mod(c1%id, 2_8**34-16)
    g1%name = 'g1'//c1%name(3:)
    g1%cx = (/c1%data, g1%data/)

    !! verify the results
    if (.not. precision_r8 (g1%data, 1.12d1**3)) error stop 1_4

    if (g1%id /= 16) error stop 2_4
    if (g1%name /= 'g1 in main prog') error stop 3_4

    if (.not. precision_x6(g1%cx(15), (1.12d1, 0))) error stop 4_4
    if (.not. precision_x6(g1%cx(16), (1.12d1**3, 0))) error stop 5_4
end
