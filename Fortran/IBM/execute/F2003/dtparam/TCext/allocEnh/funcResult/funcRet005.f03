! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/F2003/allocEnh/funcResult/funcRet005.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/16/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test function result of derived type arrays:
!                               derived type with array component of derived
!                               type of allocatable componnet.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,8)
        integer, kind         :: k1
        integer, len          :: n1
        real(k1), allocatable :: data(:)
    end type

    type container(k2,n2,k3)    ! (4,20,8)
        integer, kind     :: k2,k3
        integer, len      :: n2
        type(base(n2,k3)) :: data(2)
    end type

    contains

    type(container(4,20,8)) function splitRealArray (d1)
        double precision, intent(in) :: d1(:,:,:)

        dimension splitRealArray(3)

        call oneDimension (d1(:,1,1), splitRealArray(1))
        call oneDimension (d1(1,:,1), splitRealArray(2))
        call oneDimension (d1(1,1,:), splitRealArray(3))


        contains

        subroutine oneDimension (d2, co1)
            double precision, intent(in) :: d2(:)
            type(container(4,*,8)), intent(out) :: co1

            double precision, allocatable :: average

            if (size(d2) == 0) return

            average = sum(d2) / size(d2)

            co1%data(1)%data = pack (d2, d2 > average)
            co1%data(2)%data = pack (d2, d2 <= average)
        end subroutine
    end function
end module

program funcRet005
use m
    type(container(4,:,8)), allocatable :: co1(:)
    real(8), allocatable :: d1(:,:,:)

    allocate (d1(10,10,10))

    d1 = reshape([(j, j=1,10**3)], [10,10,10])

    !! test 1
    co1 = splitRealArray (d1)

    !! verify co1
    if (size(co1) /= 3) error stop 1_4

    do i = 1, 3
        write (*, '(10f15.6)') co1(i)%data(1)%data
        write (*, '(10f15.6)') co1(i)%data(2)%data
    end do

    !! test 2
    deallocate (co1)
    allocate (container(4,20,8) :: co1(4))

    do i = 1, 4
        co1(i) = container(4,20,8)(base(20,8)(null()))
    end do

    co1(3:1:-1) = splitRealArray (d1)

    if (size(co1) /= 4) error stop 2_4

    do i = 1, 3
        write (*, '(10f15.6)') co1(i)%data(1)%data
        write (*, '(10f15.6)') co1(i)%data(2)%data
    end do

    if (allocated(co1(4)%data(1)%data) .or. &
        allocated(co1(4)%data(2)%data)) error stop 3_4
end

