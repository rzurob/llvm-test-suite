
module m
    type base(k)
        integer, kind :: k = 4

        type (base(k)), pointer :: next => null()
        type (base(8)), pointer :: data8 => null()
        type (base(2)), pointer :: data2 => null()
        type (base(k*10)), pointer :: data10k => null()
    end type
end module

