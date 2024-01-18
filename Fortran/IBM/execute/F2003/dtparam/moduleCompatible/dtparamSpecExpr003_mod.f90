module m
    integer, parameter :: kindVal = 8

    type stat (k, n)
        integer, kind :: k = kindVal
        integer, len :: n

        real(k) :: distr(n)

        contains

        procedure :: average => computeAvg8
        procedure :: stdev   => computeStandardDeviation8
    end type

    contains

    function computeAvg8 (s)
        class(stat(kindVal,*)), intent(in) :: s
        real(s%k) computeAvg8

        if (s%n <= 0) stop 10

        computeAvg8 = sum(s%distr) / s%n
    end function

    function computeStandardDeviation8 (s)
        class(stat(kindVal,*)), intent(in) :: s
        real(s%k) computeStandardDeviation8

        real(s%k) variance, meanVal

        if (s%n <= 1) stop 20

        variance = 0.0d0

        meanVal = s%average()

        do i = 1, s%n
            variance = variance + (s%distr(i) - meanVal) * (s%distr(i) - meanVal)
        end do

        computeStandardDeviation8 = sqrt(variance/(s%n -1))
    end function
end module

