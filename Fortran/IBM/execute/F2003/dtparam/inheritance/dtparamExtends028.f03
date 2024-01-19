! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/19/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: private base type, use the type parameter
!                               and components as public.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type name (last, first)
        integer, len :: last = 20, first = 20

        character(last) :: lastName
        character(first) :: firstName
        character(2) :: title
    end type

    type, private :: base (last, first)
        integer, len :: last = 20, first = 20

        type (name(last, first)) :: realName
        type (name) :: nickName
    end type

    type, extends(base) :: child (k)
        integer, kind :: k

        integer(k) id
    end type
end module

module m1
use m
    type, extends(child) :: gen3 (rk)
        integer, kind :: rk = 4

        real(rk) data
        character(last+first) desc
    end type
end module

program dtparamExtends028
use m1
    logical(4) precision_r4
    external precision_r4

    type (child(k=4, last=10, first=12)) c1
    type (gen3(16, 30, 8)) g1

    c1%realName = name(10, 12)('abc', 'xyz', 'Mr')
    c1%nickName = name('Unknown', 'Unknown', 'U')

    c1%id = 10

    g1%nickName = c1%nickName
    g1%realName = name(16, 30)('MOT', 'bbb', 'MR')

    g1%id = 2_8**33_8 + 10
    g1%data = sin(1.5)**2 + cos(1.5)**2
    g1%desc = 'This is a test'

    !!write the results
    print *, c1%realName, c1%nickName, c1%id
    print *, g1%realName, g1%nickName, g1%id, g1%desc

    if (.not. precision_r4 (g1%data, 1.0e0)) error stop 1_4
end
