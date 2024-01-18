
module m
    type base (k1)
        integer, kind :: k1

        integer(k1) :: id
        character(20) :: name = 'default'
    end type

    type, extends(base) :: child (k2)
        integer, kind :: k2

        real(k2), pointer :: data => null()
    end type

    type container (k)
        integer, kind :: k

        type (base(k)) b1
        class (base(k)), allocatable :: b2(:,:)
        class (child(k, 2*k)), pointer :: c1 => null()
        procedure(type(child(k, k+k))), pointer, nopass :: p => null()
    end type
end module

