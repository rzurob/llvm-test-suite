!#######################################################################
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
        real(kind(2)) function addl1r1 (l1, r1)
        import
        implicit none
            logical(KIND(1)), intent(in) :: l1
            real(selected_real_kind(range)), intent(in) :: r1
        end function
    end interface
end module

program fmisc038
use m
    logical(4) precision_r8

    if (.not. precision_r8 (addl1r1 (.false._4, 1.5_8), 1.5_8)) error stop 1_4

    if (.not. precision_r8 (addl1r1 (.true._4, 2.4_8), 3.4_8)) error stop 2_4
end

real(kind(2)) function addl1r1 (l1, r1)
use m, only : KIND, range
    logical(KIND(1)), intent(in) :: l1
    real(selected_real_kind(range)), intent(in) :: r1

    addl1r1 = r1

    if (l1) addl1r1 = addl1r1 + 1.0
end function
