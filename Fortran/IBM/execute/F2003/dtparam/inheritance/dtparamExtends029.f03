! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/19/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: Explicit use of public in derived type
!                               defintion makes the derived type accessible.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    private     !<-- default to private

    type base(k)
        integer, kind :: k

        private

        real(k), allocatable, public :: data
    end type

    type, public, extends(base) :: child (l)
        integer, len :: l

        private

        integer(k), public :: id = -1
        character(l), public :: name
    end type
end module

program dtparamExtends029
use m
    type, extends(child) :: gen3 (n)
        integer, len :: n

        complex(k) cx(l: l+n-1)
    end type

    logical(4) precision_r8, precision_x6

    type (child(k=8, l=20)) c1
    type (gen3(8, 15, n=2)) g1

    c1 = child(8, 20)(1.12d1, 2_8**34, name='c1 in main program')

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
