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

    type test
        class(*), pointer :: data (:)
    end type

    type test1
        class(*), pointer :: data
    end type

    type (test) :: t1
    type (test1) :: t2
    integer*4, target :: i, i2(10)

    !! NOTE the error messages for the next two statements may change from
    !release to release
    associate (x => test (i), x1 => test1 (data = i2))   !<-- illegal
    end associate
end
