! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-19
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : another case for defect 381903
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
    real, save :: x(100)[*]
    integer me, np

    me = this_image()
    np = num_images()

    x = 0.0

    sync all
    call foo (x(10))

    if (me == 1) print *, x(10)

    contains

    subroutine foo (y)
        real y[*]

        if (this_image() == num_images()) then
            y[1] = 1.0
            print *, y[1]
        end if
        sync all
    end subroutine
    end
