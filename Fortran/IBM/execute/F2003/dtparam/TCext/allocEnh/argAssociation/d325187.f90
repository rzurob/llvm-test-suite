! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/allocEnh/argAssociation/d325187.f
! opt variations: -ql

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

    type A(k1)    ! (4)
        integer, kind            :: k1
        logical(k1), allocatable :: flag
    end type

    type(A(4)) a1(10)

    do i = 1, 10
        a1(i)%flag = mod(i,2) == 0
    end do

    !! verify
    do i = 1, 10, 2
        if (a1(i)%flag .or. (.not. a1(i+1)%flag)) error stop 1_4
    end do
    end
