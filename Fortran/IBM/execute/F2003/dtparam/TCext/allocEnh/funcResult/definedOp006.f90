! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/funcResult/definedOp006.f
! opt variations: -qnol -qnodeferredlp

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
!*  DATE                       : 09/25/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the logical allocatable as function results
!                               for floating point equalty; return type is
!                               logical.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,8)
        integer, kind         :: k1
        integer, len          :: n1
        real(k1), allocatable :: data(:)

        contains

        procedure :: baseDataEqual
        generic :: operator(==) => baseDataEqual
    end type

    contains

    logical function baseDataEqual (b1, b2)
        allocatable baseDataEqual(:)
        class (base(*,8)), intent(in) :: b1, b2

        logical(4), external :: precision_r8

        if (allocated(b1%data)) then
            if (allocated(b2%data)) then
                baseDataEqual = [(.false., i=1,max(size(b2%data), size(b1%data)))]

                do i = 1, min (size(b1%data), size(b2%data))
                    baseDataEqual(i) = &
                        precision_r8(b1%data(lbound(b1%data,1)+i-1), &
                            b2%data(lbound(b2%data,1)+i-1))
                end do
            else
                baseDataEqual = [(.false., i=1,size(b1%data))]
            end if
        else
            if (allocated(b2%data)) then
                baseDataEqual = [(.false., i=1,size(b2%data))]
            else
                baseDataEqual = [logical :: ]
            end if
        end if
    end function
end module

program definedOp006
use m
    type(base(20,8)) b1 
    type(base(:,8)) b2
    allocatable b2

    logical, allocatable :: l1(:)

    allocate (b1%data(0:99))

    b1%data = [(i, i=1,100)]

    b2 = base(20,8)(null())

    !! test 1: null data compared to null data
    l1 = b2 == b2

    if ((.not. allocated(l1)) .or. (size(l1) /= 0)) error stop 1_4

    !! test 2: non-null compared to null data
    l1 = b1 == b2

    if ((lbound(l1, 1) /= 1) .or. (ubound(l1,1) /= 100)) error stop 2_4

    if (any(l1)) error stop 3_4

    !! test 3: null data compared to non-null data
    l1 = b2 == b1

    if ((lbound(l1, 1) /= 1) .or. (ubound(l1,1) /= 100)) error stop 4_4

    if (any(l1)) error stop 5_4

    !! test 4: non-null to non-null
    b2%data = [(i, i=1,50)]

    l1 = b1 == b2

    if ((lbound(l1, 1) /= 1) .or. (ubound(l1,1) /= 100)) error stop 6_4

    if ((.not. all(l1(1:50))) .or. any(l1(51:100))) error stop 7_4
end
