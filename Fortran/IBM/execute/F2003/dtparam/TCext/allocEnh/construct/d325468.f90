! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qreuse=self /tstdev/F2003/allocEnh/construct/d325468.f
! opt variations: -qck -qnok -qnol -qdefaultpv -qreuse=none

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/18/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 325468)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        character(:), allocatable :: str
    end type

    type B(k2,n2)    ! (4,20)
        integer, kind  :: k2
        integer, len   :: n2
        type(A(k2,n2))    a1(2)
    end type
end module

use m
    type(A(4,20)) s1(2)
    type(B(4,20)) b1

    s1(1)%str = 'xyz'
    s1(2)%str = 'abcdefg'

    b1 = B(4,20)(s1)

    if ((.not. allocated(b1%a1(1)%str)) .or. &
        (.not. allocated(b1%a1(2)%str))) error stop 1_4

    if ((b1%a1(1)%str%len /= 3) .or. (b1%a1(2)%str%len /= 7)) &
        error stop 2_4

    if ((b1%a1(1)%str /= 'xyz') .or. (b1%a1(2)%str /= 'abcdefg')) &
        error stop 3_4
end
