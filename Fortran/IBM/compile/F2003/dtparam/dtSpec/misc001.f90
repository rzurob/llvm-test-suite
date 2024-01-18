!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/03/2006
!*
!*  DESCRIPTION                : miscellenous (defect 315805)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base10! (n)
        integer :: n = 10
        complex :: cx(10)

        contains

        procedure :: add => addBase10_20
    end type

    type base20! (n)
        integer :: n = 20
        complex :: cx(20)
    end type

    contains

    function addBase10_20 (b1, b2)
        class(base10), intent(in) :: b1
        class(base20), intent(in) :: b2

        class(base20), pointer :: addBase10_20

        complex temps(max(b1%n, b2%n))

        if (b1%n >= b2%n) then
            temps = b1(:b2%n)%cx + b2%cx

            temps(b2%n+1:) = b1(b2%n+1:)%cx
        else
            temps = b2(:b1%n)%cx + b1%cx

            temps(b1%n+1:) = b2(b1%n+1:)%cx
        end if

        allocate (base20 :: addBase10_20)

        addBase10_20%cx = temps
    end function
end module

program dtSpec008a1
end
