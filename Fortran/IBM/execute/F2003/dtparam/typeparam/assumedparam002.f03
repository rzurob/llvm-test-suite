
module m
    type base (l)
        integer, len :: l

        character(l) :: name = 'default'
        character(2*l) :: desc = 'blank'
    end type

    interface
        function getBaseVal (b)
        import
            type(base(*)), intent(in) :: b
            character(3*b%l+1) getBaseVal
        end function
    end interface
end module

program assumedparam002
use m
    type (base(6)) b1
    type (base(:)), allocatable :: b2(:)

    if (getBaseVal(b1) /= 'defaul;blank') error stop 1_4

    allocate (base(10) :: b2(0:9))

    b2%name = (/('b2: '//char(ichar('0')+i), i=0, 9)/)
    b2%desc = (/('b2 array, element '//char(ichar('0')+i), i=0, 9)/)

    do i = 0, 9
        print *, getBaseVal (b2(i))
    end do
end

function getBaseVal (b)
use m, only: base
    type(base(*)), intent(in) :: b
    character(3*b%l+1) getBaseVal

    getBaseVal = b%name // ';' // b%desc
end function
