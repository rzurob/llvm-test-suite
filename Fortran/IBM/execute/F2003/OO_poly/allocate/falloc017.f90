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
!*
!*  DATE                       : 09/02/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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
    type base
        class(*), allocatable :: data(:,:)
    end type

    type bigger
        type(base) :: b1
    end type

    type large
        class (bigger), allocatable :: data
    end type
end module

program falloc017
use m
    class (base), allocatable :: b1, b2(:)

    class (bigger), pointer :: bg1, bg2(:)

    class (large), allocatable :: l1, l2(:,:)

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
