! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv /tstdev/F2003/allocEnh/construct/d325085.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=self

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/11/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 325085)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A(k1)    ! (4)
        integer, kind            :: k1
        integer(k1), allocatable :: id
    end type

    type base(k2)    ! (4)
        integer, kind            :: k2
        type(A(k2)), allocatable :: data
    end type
end module

use m
    type (base(4)), allocatable :: b1(:,:)
    integer, parameter :: dim1 = 300, dim2 = 200

    allocate (b1(dim1, dim2))

    !$OMP parallel do private(i,j)
    do i = 1, dim1
        do j = 1, dim2
            allocate (b1(i,j)%data)
            b1(i,j)%data = A(4)(-1)

            b1(i,j)%data%id = i*j
        end do
    end do

    do i = 1, dim1
        do j = 1, dim2
            if (b1(i,j)%data%id /= i*j) stop 1
        end do
    end do
end
