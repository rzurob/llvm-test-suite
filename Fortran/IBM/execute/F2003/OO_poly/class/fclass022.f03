! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/07/2005
!*
!*  DESCRIPTION                : CLASS keyword (defined operator can be used for
!                                pointers)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(4) data
    end type

    interface operator (==)
        logical(4) function equal (b1, b2)
        import
            class (base), pointer, intent(in) :: b1, b2
        end function
    end interface
end module

program fclass022
use m
    class(base), pointer :: b1, b2

    nullify (b1, b2)

    if (b1 == b2) error stop 1_4

    allocate (b1, source=base(1.234_4))
    allocate (b2)

    b2%data = b1%data * 32.15_4 / 32.15_4

    if (.not. (b1 == b2)) error stop 2_4

    if (b1 == null(b1)) error stop 3_4

    if (null(b1) == null(b2)) error stop 4_4
end


logical(4) function equal (b1, b2)
use m, only: base
    class (base), pointer, intent(in) :: b1, b2
    logical(4) precision_r4
    external precision_r4

    if ((.not. associated(b1)) .or. (.not. associated (b2))) then
        equal = .false.
    else
        equal = precision_r4 (b1%data, b2%data)
    end if
end function