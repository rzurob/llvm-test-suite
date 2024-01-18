! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 2010-09-15
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : defect 380761
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
program d380761
    implicit none
    character(15), save :: msg[0:*]
    integer :: me, np

    me = this_image()
    np = num_images()

    if (np == 1) error stop 'need at least two images'

    msg = 'Img:'

    if (me /= np) then
        write (msg(5:),'(i10)') me
    else
        write (msg(5:), '(A)') ' Last'
    end if

    if (me == np) print *, msg

    call foo (msg)

    contains

    subroutine foo (c)
        character(*), intent(in) :: c[*]
        if (this_image() == 1) then
            print *, c
        end if
    end subroutine
end
