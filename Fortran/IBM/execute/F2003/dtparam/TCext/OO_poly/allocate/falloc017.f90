! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc017.f
!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc017.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 09/02/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : ALLOCATE (when an object of derived type is
!                               created by ALLOCATE statement, any allocatable
!                               ultimate components have an allocation status of
!                               unallocated)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), allocatable :: data(:,:)
    end type

    type bigger(k2,n2)    ! (4,20)
        integer, kind     :: k2
        integer, len      :: n2
        type(base(k2,n2)) :: b1
    end type

    type large(k3,n3)    ! (4,20)
        integer, kind                     :: k3
        integer, len                      :: n3
        class(bigger(k3,n3)), allocatable :: data
    end type
end module

program falloc017
use m
    class (base(4,20)), allocatable :: b1, b2(:)

    class (bigger(4,20)), pointer :: bg1, bg2(:)

    class (large(4,20)), allocatable :: l1, l2(:,:)

    !! allocation of the objects will not allocate the allocatable ultimate
    !components
    allocate (b1, b2(2:3))
    allocate (bg1, bg2(-1:1))

    if (allocated (b1%data) .or. allocated (bg1%b1%data)) error stop 1_4

    do i = 2, 3
        if (allocated (b2(i)%data)) error stop 2_4
    end do

    do i = -1, 1
        if (allocated (bg2(i)%b1%data)) error stop 3_4
    end do

    allocate (l1, l2(2,0:1))

    if (allocated (l1%data)) error stop 4_4

    do i = 1, 2
        do j = 0, 1
            if (allocated (l2(i,j)%data)) error stop 5_4
        end do
    end do

    allocate (l2(1,1)%data)

    if (allocated (l2(1,1)%data%b1%data)) error stop 6_4
end
