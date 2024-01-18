
module m
    type base (dim1, size)
        integer, len :: dim1, size

        integer :: val(dim1, size/dim1)
    end type
end module

