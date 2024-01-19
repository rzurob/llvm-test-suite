! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/20/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound procedures (Test nopass
!                               binding for using external procedure; A basic
!                               test on functionality of nopass binding.)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k,n)
        integer, kind :: k
        integer, len :: n

        integer(k) :: id
        character(n) :: name

        contains

        procedure, nopass :: equal => compareB4B8
    end type

    interface
        logical function compareB4B8 (b4, b8)
            import
            type(base(4,*)), intent(in) :: b4
            type(base(8,*)), intent(in) :: b8
        end function
    end interface
end module

program dtpNopass001
use m
    class(base(4,:)), allocatable :: b4
    type(base(8,:)), pointer :: b8

    type(base(1,1)) b1(10)

    allocate (base(4, 100):: b4)

    allocate (b8, source=base(8, 20)(10, 'xlftest'))

    b4%id = 10
    b4%name = 'xlftest'

    if (.not. b4%equal(b4, b8)) error stop 1_4
    if (.not. b8%equal(b4, b8)) error stop 2_4

    if (.not. b1%equal(b4, b8)) error stop 3_4

    b8%name = '101'

    if (b4%equal(b4, b8)) error stop 4_4
end

logical function compareB4B8 (b4, b8)
use m, only : base
    type(base(4,*)), intent(in) :: b4
    type(base(8,*)), intent(in) :: b8

    compareB4B8 = (b4%id == b8%id) .and. (b4%name == b8%name)
end function
