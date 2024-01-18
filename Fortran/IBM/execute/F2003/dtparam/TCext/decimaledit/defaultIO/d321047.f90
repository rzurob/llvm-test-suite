! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/decimaledit/defaultIO/d321047.f
! opt variations: -qnok -qnol -qnodeferredlp

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
!*  DATE                       : 06/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 321047)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), allocatable :: data
    end type
end module

use m
    class(base(4,:)), allocatable :: b1(:)

    allocate(base(4,20) :: b1(30))

    do i = 21, 30
        allocate(b1(i)%data, source=mod(i,2)==0)
    end do

    do i = 1, 10
        select type (x => b1(i+20)%data)
            type is (logical)
                if (x .neqv. mod(i,2)==0) stop 9

            class default
                stop 10
        end select
    end do
end
