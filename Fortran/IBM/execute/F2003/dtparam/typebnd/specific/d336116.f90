! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/25/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               miscellaneous (defect 336116)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data
        character(n) :: name
    end type

    type, extends(base) :: child
        integer(k) :: val(n)
    end type
end module

use m
    type(child(8,10)) c1, c2

    logical(4), external :: precision_r8

    c2%data = log(1.232d2)
    c2%name = 'xlftest'

    do i = 1, c2%n
        c2%val(i) = iachar(c2%name(i:i))
    end do

    c1 = c2

    if (.not. precision_r8(c1%data, 4.81380905109941981d0)) error stop 1_4

    if (c1%name /= 'xlftest') error stop 2_4

    if (any(c1%val /= [120, 108, 102, 116, 101, 115, 116, 32, 32, 32])) &
        error stop 3_4
end
