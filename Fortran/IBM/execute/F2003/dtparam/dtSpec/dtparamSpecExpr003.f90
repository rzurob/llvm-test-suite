!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 02/21/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Use of parametrized derived type for
!                               computation;  variable length parameter in a
!                               local scope.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

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

program dtparamSpecExpr003
    real(8), allocatable :: d1(:)

    real(8), parameter :: pi = 3.14159265358979d0

    real(8) delta, center, factor

    logical(4), external :: precision_r8
    integer i1

    interface
        real(8) function getAvg (d)
            real(8), intent(in) :: d(:)
        end function
    end interface

    delta = 1.2d0
    center = 3.5d0
    factor = 1.0d3

    call generateDistribution(d1, 200)


    if (.not. precision_r8(getAvg (d1), sum(d1)/200)) error stop 1_4

    i1 = 1000

    call generateDistribution(d1, i1)

    if (.not. precision_r8(getAvg (d1), sum(d1)/i1)) error stop 2_4

    contains

    subroutine generateDistribution (d, i)
        real(8), allocatable, intent(out) :: d(:)
        integer, intent(in) :: i

        real(8) x(2), dx

        allocate(d(i))

        x = (/center-4.0d0*delta, center+4.0d0*delta/)
        dx = (x(2) - x(1)) / (i - 1)

        do j = 1, i
            d(j) = factor * exp(-(x(1)+(j-1)*dx-center)**2/2.0d0/delta/delta) &
                / delta / sqrt(2*pi)
        end do
    end subroutine
end


real(8) function getAvg (d)
use m
    real(8), intent(in) :: d(:)

    type (stat(kindVal, size(d))) :: localStat

    localStat%distr = d

    getAvg = localStat%average()
end function

