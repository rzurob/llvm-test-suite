! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/20/2005
!*
!*  DESCRIPTION                : miscellaneous (defect 303809)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    integer, parameter :: KIND(2) = (/4, 8/)
    integer, parameter :: range = 10

    interface operator(+)
        integer(kind(2)) function addl1r1 (l1, r1)
        import
        implicit none
            logical(KIND(1)), intent(in) :: l1
            integer(selected_int_kind(range)), intent(in) :: r1
        end function
    end interface
end module


program fmisc038_1
use m
    if (.true._4 + 10_8 /= 11_8) error stop 1_4

    if (.false._4 + (-1_8) /= -1_8) error stop 2_4
end


integer(kind(2)) function addl1r1 (l1, r1)
use m, only : KIND, range
    logical(KIND(1)), intent(in) :: l1
    integer(selected_int_kind(range)), intent(in) :: r1

    addl1r1 = r1

    if (l1) addl1r1 = addl1r1 + 1
end function
