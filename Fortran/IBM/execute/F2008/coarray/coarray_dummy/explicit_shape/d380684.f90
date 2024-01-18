! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 2010-08-23
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : defect 380684
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

    implicit none
    integer, codimension[*], save :: x(3)

    integer sum3

    if (this_image() == 1) then
        x(:) = [1, 2, 3]
    end if

    sync all

    sum3 = x(1)[1] + x(2)[1] + x(3)[1]

    if (sum3 /= 6) then
        print *, 'verification fails on image', this_image()
        print *, sum3, 'vs', 6
        error stop 1
    end if
    end
