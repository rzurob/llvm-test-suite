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
!*  DATE                       : 08/30/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 324697)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(8), allocatable :: data(:)
    end type

    contains

    elemental type(base) function commonBase (b1, b2)
        type(base), intent(in) :: b1, b2

        if ((.not. allocated(b1%data)) .or. (.not. allocated(b2%data))) return

        commonBase = base(commonInteger(b1%data, b2%data))
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
    type (base), allocatable :: b1(:), b2(:)

    allocate (b1(2), b2(2))
    b1 = (/(base((/(i*100+j, j=1,10)/)), i=1,2)/)

    b2 = commonBase(b1, base((/101/)))

    if ((.not. allocated(b2(1)%data)) .or. (.not. allocated(b2(2)%data))) &
        error stop 1_4

    if ((size(b2(1)%data) /= 1) .or. (size(b2(2)%data) /= 0)) error stop 2_4

    if (b2(1)%data(1) /= 101) error stop 3_4
end
