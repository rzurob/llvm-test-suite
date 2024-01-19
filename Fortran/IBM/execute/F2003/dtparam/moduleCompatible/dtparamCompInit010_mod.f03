
module m
    type base
        integer :: id(10)
    end type

    type collector (num)
        integer, len :: num

        type (base) :: data(num)! = base((/(i, i=num,num+9)/))
    end type

    interface collector
        module procedure createCollector
    end interface

    contains

    function createCollector (n)
        integer, intent(in) :: n
        type(collector(n)) createCollector

        createCollector%data = base((/(i, i=n,n+9)/))
    end function
end module
