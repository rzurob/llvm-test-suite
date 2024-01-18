! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=self /tstdev/F2003/decimaledit/defaultIO/decEditDesc007.f
! opt variations: -ql -qreuse=none

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/21/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Mix the use of DC/DP and SS/SP in IO.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,k2)    ! (4,8)
        integer, kind :: k1,k2
        integer(k1)   :: id
        real(k2)      :: val
        logical(k1)   :: flag
        complex(k2)   :: cx
    end type
end module

program decEditDesc007
use m
    type(base(4,8)), allocatable :: b1(:), b2(:,:)

    character(:), allocatable :: mode(:)

    logical(4), external :: precision_r8, precision_x6

100 format (DP, SS, 10(i5, 1x, d25.15, 1x, G3, 2d25.15))

200 format (DC, SP, 10(i5, 1x, d25.15, 1x, G3, 2d25.15))

    allocate (mode(2), source='UNKNOWN         ')

    open (20, access='direct', recl=1000, form='formatted', status='scratch',&
            sign='suppress')

    inquire(20, decimal=mode(1), sign=mode(2))

    if ((mode(1) /= 'POINT') .or. (mode(2) /= 'SUPPRESS')) error stop 1_4

    allocate (b2(10,10))

    allocate(b1(100), source=(/(base(4,8)(i, i*1.2d0, mod(i,2) == 0, i*2.4d0), &
                i=1,100)/))

    do i = 1, 10, 2
        write (20, '(DC, SP, 10(i5, 1x, d25.15, 1x, G3, 2d25.15))', rec=i) &
                b1(i*10-9:i*10)

        write (20, 100, rec=i+1) b1(i*10+1:i*10+10)
    end do

    !! now read in the data
    do i = 1, 10, 2
        read (20, rec=i, fmt=200) b2(:,i)

        read (20, rec=i+1, fmt='(10(i5, 1x, d25.15, 1x, G3, 2d25.15))') &
                b2(:,i+1)
    end do

    close(20)

    !! verify the results

    k = 1

    do i = 1, 10
        do j = 1, 10
            if (b2(j,i)%id /= k) error stop 1_4

            if (.not. precision_r8(b2(j,i)%val, k*1.2d0)) error stop 2_4

            if (b2(j,i)%flag .neqv. mod(k,2) == 0) error stop 3_4

            if (.not. precision_x6(b2(j,i)%cx, cmplx(k*2.4d0, kind=8))) &
                    error stop 4_4
            k = k + 1
        end do
    end do
end
