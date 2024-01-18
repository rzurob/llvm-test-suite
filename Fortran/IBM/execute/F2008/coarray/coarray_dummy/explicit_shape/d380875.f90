! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-09-20
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : defect 380875
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

        x(me)[co_n] = cmplx(1, 1)*me
        sync all
    end subroutine
end module

program d380875
    use test, only : set_array
    implicit none

    complex, save :: data1(10000)[*]
    integer me, np, i

    call set_array (data1, size(data1), this_image())

    ! verify on image 1
    me = this_image()
    np = num_images()

    if (me == 1) then
        call verifyData (data1, np)
    end if
end

subroutine verifyData (cx, n)
    implicit none
    integer, intent(in) :: n
    complex, intent(in) :: cx(n)

    logical, external :: precision_x8
    integer i

    do i = 1, n
        if (.not. precision_x8(cx(i), cmplx(1,1)*i)) then
            print *, 'failed to verify on element', i
            print *, cx(i), 'vs', cmplx(1,1)*i
            error stop 1
        end if
    end do
end subroutine

