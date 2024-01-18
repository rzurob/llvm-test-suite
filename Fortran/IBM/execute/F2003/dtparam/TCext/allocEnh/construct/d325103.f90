! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/construct/d325103.f
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
!*  DATE                       : 09/12/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               miscellaneous (defect 325103)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    type A(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      id
    end type

    integer, allocatable :: i1(:)

    type(A(:,4)), allocatable :: a1(:)

    allocate (a1(-1:98), source=(/(A(20,4)(i), i=-1,98)/))

    allocate (i1(-1:98), source=(/(i, i=-1,98)/))

    a1 = a1(::2)
    i1 = i1(::2)

    if ((size(i1) /= 50) .or. (size(a1) /= 50)) error stop 1_4

    do i = 1, 50
        if (a1(i)%id /= 2*i-3) error stop 2_4
        if (i1(i) /= 2*i-3) error stop 3_4
    end do
    end
