! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/zeroSize/d324697.f
! opt variations: -qnol -qnodeferredlp

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/30/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 324697)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,8)
        integer, kind            :: k1
        integer, len             :: n1
        integer(k1), allocatable :: data(:)
    end type

    contains

    elemental type(base(20,8)) function commonBase (b1, b2)
        type(base(*,8)), intent(in) :: b1, b2

        if ((.not. allocated(b1%data)) .or. (.not. allocated(b2%data))) return

        commonBase = base(20,8)(commonInteger(b1%data, b2%data))
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

        allocate(commonInteger(icount)); commonInteger = bucket(:icount)
    end function
end module

program zeroSizeArray010a
use m
    type (base(:,8)), allocatable :: b1(:), b2(:)

    allocate (base(20,8) :: b1(2), b2(2))
    b1 = (/(base(20,8)((/(i*100+j, j=1,10)/)), i=1,2)/)

    b2 = commonBase(b1, base(20,8)((/101/)))

    if ((.not. allocated(b2(1)%data)) .or. (.not. allocated(b2(2)%data))) &
        error stop 1_4

    if ((size(b2(1)%data) /= 1) .or. (size(b2(2)%data) /= 0)) error stop 2_4

    if (b2(1)%data(1) /= 101) error stop 3_4
end
