!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/06/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Test the type parameter order when no type
!                               parameter keyword is used.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(l, k, n)
        integer, len :: l, n=100
        integer, kind :: k
    end type

    type, extends(base) :: child (unnecessarylongnamefortypeparam)
        integer, len :: unnecessarylongnamefortypeparam
    end type

    type, extends(child) :: gen3
    end type

    type, extends(gen3) :: gen4 (p, q, r)
        integer, kind :: q
        integer, len :: p, r

        integer(k) :: id(n)
        character(l) :: name

        complex(k) :: cx(unnecessarylongnamefortypeparam)
    end type
end module

program typeParamOrder001
use m
    !l=20,k=8,n=10, unnecessarylongnamefortypeparam=6,p=1000, q=7,r=-1
    class (gen4(20, 8, 10, 6, 1000, 7, -1)), allocatable :: g4_1(:,:,:)

    !l=10, k=4, n=200, unnecessarylongnamefortypeparam=-10
    type (child(10,4,200, -10)), pointer :: c1

    !l=-10, k=15,n=-5,unnecessarylongnamefortypeparam=8
    type (gen3(-10,15,-5, 8)) g3_1(80:100)

    class(base(26, 8, 1000)), pointer :: b1(:,:)

    allocate (g4_1(2, 3, 4), c1, b1(300, 2))

    !! verify the type parameters
    if ((b1%l /= 26) .or. (b1%k /= 8) .or. (b1%n /= 1000)) error stop 1_4

    if ((c1%l /= 10) .or. (c1%k /= 4) .or. (c1%n /= 200) .or. &
        (c1%unnecessarylongnamefortypeparam /= -10)) error stop 2_4

    if ((g3_1%l /= -10) .or. (g3_1%k /= 15) .or. (g3_1%n /= -5) .or. &
        (g3_1%unnecessarylongnamefortypeparam /= 8)) error stop 3_4

    if ((g4_1%l /= 20) .or. (g4_1%k /= 8) .or. (g4_1%n /= 10) .or. &
        (g4_1%unnecessarylongnamefortypeparam /=6) .or. &
        (g4_1%p /= 1000) .or. (g4_1%q /= 7) .or. (g4_1%r /= -1)) error stop 4_4


    if ((kind(g4_1(2,3,4)%id) /= 8) .or. (size(g4_1(1,1,1)%id) /= 10) .or. &
        (len(g4_1%name) /= 20) .or. (kind(g4_1(2,1,4)%cx) /= 8) .or. &
        (size(g4_1(1,3,2)%cx) /= 6)) error stop 5_4
end
