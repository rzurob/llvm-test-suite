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
!*  DATE                       : 03/21/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Structure constructor used for the
!                               source-expr in allocate statement.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        complex(k) :: cx(n)
    end type

    type container (k)
        integer, kind :: k

        type(base(k,:)), pointer :: data(:)
    end type

    contains

    function produceBasePtrArray (n, m, r1, aimg1)
        integer, intent(in) :: n, m
        real(8), intent(in) :: r1(n*m), aimg1(m*n)

        type(base(8,:)), pointer :: produceBasePtrArray(:)

        allocate (base(8,n) :: produceBasePtrArray(m))

        do i = 1, m
            produceBasePtrArray(i)%cx = cmplx(r1((i-1)*n+1:i*n), &
                aimg1((i-1)*n+1:i*n), kind=8)
        end do
    end function
end module


program dtparamConstr039a
use m
    type(container(8)), allocatable :: co1

    real(8) d1(1000), d2(1000)

    logical(4), external :: precision_x6

    d1 = log(1.1d0*(/(i, i=1,1000)/))
    d2 = log(sqrt(1.1d0*(/(i, i=1,1000)/)))

    allocate(co1, source=container(8)&
            (produceBasePtrArray(150, 6, d1, d2(1:900))))


    !! verify co1
    if (size(co1%data) /= 6) error stop 1_4

    k = 1

    do i = 1, 6
        do j = 1, 150
            if (.not. precision_x6(co1%data(i)%cx(j), cmplx(log(1.1d0*k), &
                .5d0*log(1.1d0*k), kind=8))) error stop 2_4

            k = k + 1
        end do
    end do
end
