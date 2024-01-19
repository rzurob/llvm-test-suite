! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/zeroSize/zeroSizeArray010a.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/30/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Similar test case as zeroSizeArray010, now try
!                               an array of rank 7 for the derived type; and use
!                               the intrinsic assignment for an allocatable
!                               entity of rank 7; expression is the user defined
!                               operator.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,8)
        integer, kind            :: k1
        integer, len             :: n1
        integer(k1), allocatable :: data(:)

        contains

        procedure, private :: commonBase
        generic :: operator(.join.) => commonBase
    end type

    contains

    elemental type(base(20,8)) function commonBase (b1, b2)
        class(base(*,8)), intent(in) :: b1, b2

        if ((.not. allocated(b1%data)) .or. (.not. allocated(b2%data))) return

        commonBase%data = commonInteger(b1%data, b2%data)
    end function

    pure function commonInteger (i1, i2)
        integer(8), intent(in) :: i1(:), i2(:)

        integer(8) bucket(min(size(i1), size(i2))), icount

        integer(8), allocatable :: commonInteger(:)

        icount = 0

        do i = 1, size(i1)
            do j = 1, size(i2)
                if (i1(i) == i2(j)) then
                    icount = icount + 1
                    bucket(icount) = i1(i)
                end if
            end do
        end do

        commonInteger = bucket(:icount)
    end function
end module

program zeroSizeArray010a
use m
    type (base(:,8)), allocatable :: b1(:,:,:,:,:,:,:), b2(:,:,:,:,:,:,:)

    b1 = reshape((/(base(20,8)((/(i*1000+j, j=1,100)/)), i=1,128)/), (/2,2,2,2,2,2,2/))

    b2 = b1 .join. base(20,8)((/1001, 1010, 128001/))


    !! verify b2
    if (any(shape(b2) /= (/2,2,2,2,2,2,2/))) error stop 1_4

    if (size(b2(1,1,1,1,1,1,1)%data) /= 2) error stop 2_4
    if (size(b2(2,2,2,2,2,2,2)%data) /= 1) error stop 3_4

    if (b2(1,1,1,1,1,1,1)%data(1) /= 1001) error stop 4_4
    if (b2(1,1,1,1,1,1,1)%data(2) /= 1010) error stop 5_4

    if (b2(2,2,2,2,2,2,2)%data(1) /= 128001) error stop 6_4

    do i1 = 1, 2
        do i2 = 1, 2
            do i3 = 1, 2
                do i4 = 1, 2
                    do i5 = 1,2
                        do i6 = 1, 2
                            do i7 = 1, 2
                                if (.not.  allocated(b2(i1,i2,i3,i4,i5,i6,i7)%data)) &
                                        error stop 7_4

                                if (size(b2(i1,i2,i3,i4,i5,i6,i7)%data) /= 0) then
                                    print *, i1,i2,i3,i4,i5,i6,i7
                                    print *, b2(i1,i2,i3,i4,i5,i6,i7)%data
                                end if
                            end do
                        end do
                    end do
                end do
            end do
        end do
    end do
end
