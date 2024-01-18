! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-09-02
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : defect 380757
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
program d380757
    implicit none
    character(10), save :: comsg[*]
    character(:), allocatable :: message

    write (comsg, '(A,i9)') ':', this_image()
    sync all

    if (this_image() == 3) then
        allocate (character(20) :: message)
        message = comsg[1] // comsg[2]
    end if
    sync memory

    if (this_image() == 3) then
        if (message /= ':        1:        2') then
            print *, 'verification on image 3 failed'
            error stop 1
        end if
    else
        if (allocated(message)) then
            print *, 'verification on image', this_image(), 'failed'
            print *, 'message shouldn"t be allocated'
            error stop 2
        end if
    end if
end

