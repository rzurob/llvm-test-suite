! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/12/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               miscellaneous (defect 325103)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    type A
        integer id
    end type

    integer, allocatable :: i1(:)

    type(A), allocatable :: a1(:)

    allocate (a1(-1:98), source=(/(A(i), i=-1,98)/))

    allocate (i1(-1:98), source=(/(i, i=-1,98)/))

    a1 = a1(::2)
    i1 = i1(::2)

    if ((size(i1) /= 50) .or. (size(a1) /= 50)) error stop 1_4

    do i = 1, 50
        if (a1(i)%id /= 2*i-3) error stop 2_4
        if (i1(i) /= 2*i-3) error stop 3_4
    end do
    end