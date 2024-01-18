! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/27/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : select type (use to define assignment for
!                               derived type with unlimited poly-allocatable
!                               scalar component)
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
        class(*), allocatable :: data
    end type

    interface assignment(=)
        subroutine assgnB1B2 (b1, b2)
        import base
            type (base), intent(inout) :: b1
            type (base), intent(in) :: b2
        end subroutine
    end interface
end module

program fselTyp506a
use m
    type (base) :: b1, b2
    type (base), allocatable :: b3(:), b4(:)

    allocate (b1%data, source=100.5)

    !! invoke the defined assignment, will turn real/complex into integer
    b2 = b1

    select type (x => b2%data)
        type is (integer)
            if (x /= 100) error stop 1_4
        type is (real)
            error stop 2_4
        class default
            error stop 3_4
    end select

    deallocate (b2%data)

    allocate (b2%data, source='xlftest')

    !! the defined assignment will use the intrinsic assignment
    b1 = b2

    select type (x => b1%data)
        type is (character(*))
            if (x /= 'xlftest') error stop 4_4
        class default
            error stop 5_4
    end select
end


subroutine assgnB1B2 (b1, b2)
use m, only : base
    type (base), intent(out) :: b1
    type (base), intent(in) :: b2

    if (.not. allocated (b2%data)) return

    select type (x => b2%data)
        type is (real)
            allocate (b1%data, source=int(x))

        type is (complex)
            allocate (b1%data, source=int(x))

        class default
            b1 = b2     !<-- use intrinsic assignment
    end select
end subroutine
