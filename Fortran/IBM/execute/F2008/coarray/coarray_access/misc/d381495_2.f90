! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 2010-09-27
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : defect 381495.2
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

program d381495_2
    implicit none
    real(8), save :: x(10)[*]

    call random_number (x)

    call verify_x (x)

    contains

    subroutine verify_x (x1)
        real(8), intent(in) :: x1(10)[*]

        integer i, j, img

        do i = 1, 10

            if (i /= 1) then
            end if
        end do
    end subroutine
end

