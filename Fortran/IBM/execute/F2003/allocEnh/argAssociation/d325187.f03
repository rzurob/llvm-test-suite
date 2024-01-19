! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/13/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               miscellaneous (defect 325187)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    type A
        logical, allocatable :: flag
    end type

    type(A) a1(10)

    do i = 1, 10
        a1(i)%flag = mod(i,2) == 0
    end do

    !! verify
    do i = 1, 10, 2
        if (a1(i)%flag .or. (.not. a1(i+1)%flag)) error stop 1_4
    end do
    end
