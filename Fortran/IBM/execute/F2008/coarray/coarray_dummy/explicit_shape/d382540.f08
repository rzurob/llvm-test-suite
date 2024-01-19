! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-11-09
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : defect 382540
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

program d382540
    implicit none
    complex, save :: d1 [*]
    integer idx(2)
    logical, external :: precision_x8

    call set_up (d1, 4)

    idx = get_idx(d1, 4)

    sync all

    print *, d1
    if (.not. precision_x8 (d1, cmplx(idx(1), idx(2),4))) then
        print *, 'Failed to verify d1 on image', this_image()
        print *, d1, 'vs', cmplx(idx(1), idx(2),4)
        error stop 1
    end if

    contains

    subroutine set_up (x, m)
        integer, intent(in) :: m
        complex, intent(out) :: x[m,*]
        integer i(2)

        i = get_idx(x, m)

        x = cmplx (i(1), i(2))
    end subroutine

    function get_idx (x, m) result(res)
        integer, intent(in) :: m
        complex, intent(in) :: x[m,*]
        integer res(2)

        res = this_image(x)
    end function
end
