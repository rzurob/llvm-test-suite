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
!*  DATE                       :
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 317114)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base4_20!(k, n)
        integer :: n = 20

        integer(4/3) :: id(20/4,4)
        real(4) :: data(20)
    end type

    type base8_350!(k, n)
        integer(8/3) :: id(350/4,4)
        real(8) :: data(350)
    end type

    interface
        function genID (i1, shape)
            integer, intent(in) :: i1(:), shape(2)
            integer, pointer :: genID(:,:)
        end function

        function genReal (r1, isize, p)
            real, intent(in) :: r1(:)
            integer, intent(in) :: isize
            procedure(real) p

            real, pointer :: genReal(:)
        end function
    end interface

    type (base4_20), allocatable :: b1_m
end module

program dtparamConstr028
use m
    type (base8_350) :: b1

    integer i1(1000), k, k1
    real r1(1000)

    intrinsic sqrt, alog10

    logical(4), external :: precision_r4

    i1 = (/(i, i=1, 1000)/)
    r1 = i1*1.0e10

    b1 = base8_350(genID(i1, (/87, 4/)), genReal(r1, 350, sqrt))

    allocate (b1_m)
    
    b1_m = base4_20(20, genID(i1(500:)/5, (/5,4/)), &
            genReal(r1(500:), 20, alog10))

    !! verify results
    if (b1_m%n /= 20) error stop 1_4

    do i = 1, 350
        if (.not. precision_r4 (real(b1%data(i), 4), sqrt(i*1.0e10))) &
                error stop 2_4
    end do

    k = 1
    k1 = 0

    do j = 1, 4
        do i = 1, 87
            if (b1%id(i,j) /= k) error stop 3_4

            k = k + 1
        end do

        do i = 1, 5
            if (b1_m%id(i,j) /= 100 + k1/5) error stop 4_4

            k1 = k1 + 1
        end do
    end do

    if ((k /= 349) .or. (k1 /= 20)) error stop 5_4

    do i = 1, 20
        if (.not. precision_r4(b1_m%data(i), 1.0e1+log10(5.0e2+i-1))) &
                error stop 6_4
    end do
end

function genID (i1, shape)
    integer, intent(in) :: i1(:), shape(2)
    integer, pointer :: genID(:,:)

    if (size(i1) < product(shape)) stop 10

    allocate(genID(shape(1), shape(2)))
    genID = reshape(i1, shape)
end function


function genReal (r1, isize, p)
    real, intent(in) :: r1(:)
    integer, intent(in) :: isize
    procedure(real) p

    real, pointer :: genReal(:)

    if (size(r1) < isize) stop 20

    allocate (genReal(isize))
    
    genReal = (/(p(r1(i)), i=1, isize)/)
end function

