! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_poly/point_assgn/fpAssgn001d4.f
! opt variations: -qnok -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/28/2005
!*
!*  DESCRIPTION                : data pointer assignment (diagnostic test case:
!                               the rank mismatch for the pointer component of a
!                               derived type in a structure constructor)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fpAssgn001d4

    type test(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), pointer :: data (:)
    end type

    type test1(k2,n2)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n2
        class(*), pointer :: data
    end type

    type (test(4,20)) :: t1
    type (test1(4,20)) :: t2
    integer*4, target :: i, i2(10)

    !! NOTE the error messages for the next two statements may change from
    !release to release
    associate (x => test(4,20) (i), x1 => test1(4,20) (data = i2))   !<-- illegal
    end associate
end
