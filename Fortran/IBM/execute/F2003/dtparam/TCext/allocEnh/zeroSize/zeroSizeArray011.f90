! GB DTP extension using:
! ftcx_dtp -qdeferredlp /tstdev/F2003/allocEnh/zeroSize/zeroSizeArray011.f
! opt variations: -qck -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/05/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Try a zero-sized array in creating an array in a
!                               subroutine.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (101,4)
        integer, kind :: k1
        integer, len  :: n1
        character(n1) :: name
        integer(k1)      id
    end type

    contains

    subroutine readArray (unit,lb,ub, b1)
        integer, intent(in) :: lb, ub, unit
        type(base(:,4)), allocatable, intent(inout) :: b1(:)

        type(base(101,4)) localCopy(lb:ub)

        read (unit, *) localCopy

        b1 = localCopy
    end subroutine
end module

program zeroSizeArray011
use m
    type (base(:,4)), allocatable :: b1(:)

    write (1, *) ('abcd   ', i, i=1,10)
    write (1, *) 'blank'

    rewind(1)

    call readArray (1, 0,9, b1)

    if ((lbound(b1,1) /= 0) .or. (ubound(b1,1) /= 9)) error stop 1_4

    do i = 0, 9
        if (b1(i)%name /= 'abcd') error stop 2_4

        if (b1(i)%id /= i+1) error stop 3_4
    end do

    call readArray (1, 10, 0, b1)

    if (.not. allocated(b1)) error stop 4_4

    if ((lbound(b1,1) /= 1) .or. (ubound(b1,1) /= 0)) error stop 5_4
end
