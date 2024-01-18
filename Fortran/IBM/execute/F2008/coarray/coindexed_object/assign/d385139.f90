! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 2011-02-22
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : defect 385139
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
    real, save :: r1[*]
    complex, save :: cx1[*]
    integer me

    logical, external :: precision_x8

    if (num_images() < 2) then
        print *, 'this program requires at least two images'
        error stop 1
    end if

    me = this_image()

    r1 = 1.0*me
    sync all

    if (me == 2) cx1[1] = cmplx(r1[1],0.0)

    sync all

    if (.not. precision_x8 (cx1[1], cmplx(1.0, 0.0))) then
        print *, 'verification on image',me,'failed'
        print *, cx1[1], 'vs', cmplx(1.0, 0.0)
        error stop 1
    end if

    end
