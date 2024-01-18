! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-19
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : defect 38064
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
    integer, save :: x[*]
    integer me

    me = this_image()

    if (me == 1) print *, this_image(x)
    call foo (x, 2)
    contains

    subroutine foo (a, m)
        integer, intent(in) :: m
        integer, intent(in) :: a[m, *]

        if (me == 1) print *, this_image(a)
    end subroutine
end
