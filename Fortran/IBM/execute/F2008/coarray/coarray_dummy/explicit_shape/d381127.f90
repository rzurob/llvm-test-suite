! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 2010-09-20
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : defect 381127
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

program d381127
    implicit none

    character(5), save :: co_str[*]

    if (num_images() < 2) then
        print *, 'need more than 1 image to run this program'
        error stop 1
    end if

    call foo (co_str)
    sync all

    if (this_image() == 2) then
        if (co_str /= 'abcd') then
            print *, 'co_str verification failed'
            print *, co_str
            error stop 1
        end if
    end if

    contains

    subroutine foo (str)
        character(*), intent(out) :: str[*]

        if (this_image() == 1) str(:)[2] = 'abcd'
    end subroutine
    end

