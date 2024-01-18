! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/allocEnh/construct/d328616.f
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
!*  DATE                       : 11/22/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 328616)
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

    contains

    function genBase (b, b2)
        class(base(4,*)), intent(in) :: b(:,:), b2(:,:)

        type(base(4,20)) genBase(size(b,1),size(b,2))

        do i = 1, size(b,1)
            do j = 1, size(b,2)
                genBase(i,j) = base(4,20)(b2(i,j)%data)
            end do
        end do
    end function
end module

use m
    class(base(4,:)), allocatable :: b1(:,:)
    type(base(4,:)), allocatable :: b2, b3(:,:)

    b2 = base(4,20)(1)

    allocate (b1(3,3), source=reshape((/(base(4,20)(i*1.2), i=1,3), &
        (base(4,20)(i), i=1,3), (base(4,20)(i*1_8), i=1,3)/), (/3,3/)))

    associate (x => genBase(reshape((/(b2, i=1,9)/),(/3,3/)), b1), &
        y => genBase(b1,reshape((/(b2, i=1,9)/),(/3,3/))))
    end associate
end
