! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-09-20
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : defect 380875.2
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
module test
    implicit none
    contains

    subroutine set_array (x, n,co_n)
        integer, intent(in) :: n, co_n
        complex, intent(out) :: x(n)[co_n:*]

        integer :: me

        me = this_image()

        x(me)[co_n] = cmplx(1, 1)
        sync all
    end subroutine
end module


program d380875_2
use test
    implicit none
    complex, save :: cx(10000)[2:*]
    integer me, np, i
    logical, external :: precision_x8

    me = this_image()
    np = num_images()
    call set_array (cx, 10000, 2)

    ! verify results on image 1
    if (me == 1) then
        do i = 1, np
            if (.not. precision_x8(cx(i), cmplx(1,1))) then
                print *, 'failed to verify element', i
                print *, cx(i), 'vs', cmplx(1,1)
                error stop 1
            end if
        end do
    end if
end

