! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp /tstdev/F2003/decimaledit/dtio/d323414.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp

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
!*  DATE                       : 07/26/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellanous (defect 323414)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type dataType(n1,k1)    ! (20,8)
        integer, kind         :: k1
        integer, len          :: n1
        real(k1), allocatable :: data(:)
    end type

    type base(k2,n2,k3)    ! (4,20,8)
        integer, kind                      :: k2,k3
        integer, len                       :: n2
        class(dataType(:,k3)), allocatable :: data(:)
    end type

end module

use m
    class(base(4,:,8)), allocatable :: b1(:)


    do i = 1, 10
        if (allocated(b1)) deallocate(b1)

        allocate (b1(i), source=base(4,20,8)(data=(/(dataType(20,8)(null()), j=1,i)/)))
    end do

    if ((.not. allocated(b1)) .or. (size(b1) /= 10)) error stop 1_4

    do i = 1, 10
        if (.not. allocated(b1(i)%data)) error stop 2_4

        if (size(b1(i)%data) /= 10) error stop 3_4

        do j = 1, 10
            if (allocated (b1(i)%data(j)%data)) error stop 4_4
        end do
    end do
end
