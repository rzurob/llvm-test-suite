! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/10/2005
!*
!*  DESCRIPTION                : poly-function return (defined operator, defined
!                               assignment)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        character(30), allocatable :: str
    end type

    interface operator(//)
        module procedure concatenate
        module procedure concatenate2
    end interface

    interface assignment(=)
        module procedure assgnB1B2
    end interface

    private concatenate

    contains

    class (base) function concatenate (b, c)
        class (base), intent(in) :: b
        character(*), intent(in) :: c
        allocatable concatenate

        allocate (concatenate)

        if (.not. allocated(b%str)) then
            allocate (concatenate%str, source = c)
        else
            allocate (concatenate%str, source = trim(b%str)//' '//c)
        end if
    end function

    class (base) function concatenate2 (b, c)
        class(base), intent(in) :: b, c
        allocatable concatenate2

        allocate (concatenate2)

        concatenate2 =  b // c%str
    end function

    subroutine assgnB1B2 (b1, b2)
        class(base), intent(in) :: b2
        class(base), intent(out) :: b1

        if (.not. same_type_as(b1, b2)) error stop 10_4

        select type (b1)
            type is (base)
                allocate (b1%str, source=b2%str)
            class default
                error stop 11_4
        end select
    end subroutine
end module

program ffuncRet009a3
use m
    class (base), pointer :: b1, b2(:)

    allocate(b1, b2(0:2))

    b1 = b1 // 'xlftest'

    b1 = b1 // 'team'

    if (b1%str /= 'xlftest team') error stop 1_4

    b2(0) = b1

    b2(1) = b2(0) // b1

    if (b2(1)%str /= 'xlftest team xlftest team') error stop 2_4
end
