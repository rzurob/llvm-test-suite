! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-09-26
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : a module method that average the field of a
!                                rank-one array
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
module compute_average_mod
    use distribute_module, only: double
    implicit none

    contains

    ! field x is average as follows: (x(i-1) + x(i) + x(i+1))/3 == y(i)
    subroutine average_field (x,y, n)
        integer, intent(in) :: n
        real(double), intent(in) :: x(n)[*]
        real(double), intent(out) :: y(n)[*]

        integer i, me, left, right, np

        me = this_image()
        np = num_images()

        do i = 2, n-1
            y (i) = (x(i-1) + x(i) + x(i+1))/3
        end do

        if (me == 1) then
            y(1) = x(1)
            y(n) = (x(n-1) + x(n) + x(1)[2])/3
        else if (me == np) then
            y(n) = x(n)
            y(1) = (x(n)[np-1] + x(1) + x(2))/3
        else
            y(1) = (x(n)[me - 1] + x(1) + x(2))/3
            y(n) = (x(n-1) + x(n) + x(1)[me + 1])/3
        end if

        sync all
    end subroutine
end module
