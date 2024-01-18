! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp /tstdev/F2003/allocEnh/argAssociation/d327270_2.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self

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
!*  DATE                       : 10/27/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               miscellaneous (defect 327270); a 2nd variance.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind     :: k1
        integer, len      :: n1
        real(k1), pointer :: data
    end type
end module

module m1
use m
    type container(k2,n2)    ! (4,20)
        integer, kind                 :: k2
        integer, len                  :: n2
        type(base(:,k2)), allocatable :: data
    end type
end module

use m1
    type(container(4,:)), allocatable :: co1(:)

    type(container(4,20)) :: co2(0:29)

    logical(4), external :: precision_r4

    do i = 0, 29
        allocate (base(20,4) :: co2(i)%data)
        allocate (co2(i)%data%data)

        co2(i)%data%data = log(i+1.5)
    end do

    co1 = co2

    co1 = [co2, co1, co2]

    do i = 1, 30
        if (.not. associated(co1(i)%data%data, co2(i-1)%data%data)) &
            error stop 2_4

        if (.not. precision_r4 (co1(i)%data%data, log(i+0.5))) error stop 10_4

        if (.not. associated(co1(30+i)%data%data, co2(i-1)%data%data)) &
            error stop 3_4


        if (.not. associated(co1(60+i)%data%data, co2(i-1)%data%data)) &
            error stop 3_4
    end do
end
