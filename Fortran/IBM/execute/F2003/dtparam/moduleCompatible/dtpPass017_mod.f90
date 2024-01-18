
module m
    type arrayTemp (n)
        integer, len :: n

        real(8) data(n)

        contains

        procedure :: equal => at1EqAt2
    end type

    contains

    logical function at1EqAt2 (at1, at2)
        class(arrayTemp(*)), intent(in) :: at1, at2

        dimension at1EqAt2 (max(at1%n, at2%n))

        logical(4), external :: precision_r8

        integer i

        at1EqAt2 = .false.

        do i = 1, min (at1%n, at2%n)
            at1EqAt2(i) = precision_r8 (at1%data(i), at2%data(i))
        end do
    end function
end module

module m1
use m
    type coArraySim (n, m)
        integer, len :: n, m

        type(arrayTemp(n)) data(m)

        contains

        procedure :: cshift => circShiftData
    end type

    contains

    function circShiftData (cas, shift)
        implicit none
        class(coArraySim(*,*)), intent(in) :: cas
        integer, intent(in) :: shift

        !type (arrayTemp(cas%n)) circShiftData(cas%m)
        type (coArraySim (cas%n,cas%m)) circShiftData

        integer i, j1, j2, j3, j4, newIndex

        !$OMP PARALLEL DO, private(j1, j2, newIndex, j3, j4)
        do i = 1, cas%m * cas%n
            j1 = (i-1)/cas%n + 1
            j2 = i - (j1-1)*cas%n

            newIndex = 1 + modulo (i+shift-1, cas%m * cas%n)

            j3 = (newIndex-1)/cas%n + 1
            j4 = newIndex - (j3-1)*cas%n
            circShiftData%data(j1)%data(j2) = cas%data(j3)%data(j4)
        end do
    end function
end module
