! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-09-26
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : define a module that distributes data across
!                               all images. Data are read in from a file.
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

module distribute_module
    implicit none

    integer, parameter :: double = selected_real_kind (14)

    contains

    subroutine distribute_data (filename, n, x)
        character(*), intent(in) :: filename
        integer, intent(in) :: n
        real(double), intent(out) :: x(n)[*]

        integer img_idx, element, i, istat
        real(double) temp

        if (this_image() == 1) then
            open (1, file=filename, status='old')

            do
                read (1, *, iostat=istat) i, temp
                if (istat /= 0) exit

                img_idx = (i-1)/n + 1
                element = i - (img_idx - 1) * n

                x(element)[img_idx] = temp
            end do

            close(1)
        end if

        sync all
    end subroutine
end module
