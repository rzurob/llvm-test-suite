!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/13/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Rank one array used for rank two array
!                               data component: parameterized derived type data.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data (n)
    end type

    type container (k, n, dim1, dim2)
        integer, kind :: k
        integer, len :: n, dim1, dim2

        type (base(k,n)) :: data(dim1, dim2)
    end type
end module

program dtparamConstr032a1
use m
    type (base(8,:)), allocatable :: b1(:)

    type (container(8,:,:,:)), allocatable :: co1

    real(8), allocatable :: r1(:)

    logical(4), external :: precision_r8

    integer n, dim1, dim2

    call prepareData (b1, co1, 25, 15, 35)

    allocate (r1(b1%n*size(b1)))

    call random_number (r1)

    b1 = (/(base(8,25)(data=r1(i*25-24:i*25)), i=1, size(b1))/)

    co1 = container(8,25,15,35)(reshape(b1, (/co1%dim1, co1%dim2/)))

    !! verify co1
    kcount = 1

    do j = 1, 35
        do i = 1, 15
            do k = 1, 25
                if (.not. precision_r8(co1%data(i,j)%data(k), r1(kcount))) &
                        error stop 1_4

                kcount = kcount + 1
            end do
        end do
    end do

    n = 35
    dim1 = 25
    dim2 = 15

    call prepareData (b1, co1, n, dim1, dim2)

    r1 = log((/(i*1.0d0, i=1, size(r1))/))

    b1 = (/(base(8,n)(r1(i*n-n+1:i*n)), i=1, size(b1))/)

    co1 = container(8,n,dim1,dim2) (data = reshape(b1, (/dim1, dim2/)))

    kcount = 1

    do j = 1, dim2
        do i = 1, dim1
            do k = 1, n
                if (.not. precision_r8(co1%data(i,j)%data(k), &
                    log(kcount*1.0d0))) error stop 2_4

                kcount = kcount + 1
            end do
        end do
    end do

    contains

    subroutine prepareData (b, co, n, dim1, dim2)
        type (base(8,:)), allocatable, intent(out) :: b(:)
        type (container(8,:,:,:)), allocatable, intent(out) :: co
        integer, intent(in) :: n, dim1, dim2

        allocate (base(8,n) :: b(dim1*dim2))
        allocate (container(8,n,dim1,dim2) :: co)
    end subroutine
end
