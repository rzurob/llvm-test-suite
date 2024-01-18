program fclass011
    type base
        class (*), allocatable :: data(:)
    end type

   type (base) b1, b2
    integer(8) i1(0:2)

    i1 = (/0, 1, 2/)
    !b1%data(lbound(i1,1):ubound(i1,1)) = i1
    allocate (b1%data(lbound(i1,1):ubound(i1,1)), source=i1)
end

