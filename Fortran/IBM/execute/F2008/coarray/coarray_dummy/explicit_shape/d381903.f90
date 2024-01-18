! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 2010-10-19
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : defect 381903
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
    real, save :: x(10)[*]

    integer me

    me = this_image()

    x = 0.5
    call foo (x(2:), 5)
    if (me == 1) print *, x

    call foo (x(1), 5)
    if (me == 2) print *, x

    call foo (x(:), 5)
    if (me == 3) print *, x

    contains

    !test the sequence association
    subroutine foo (x,n)
        integer, intent(in) :: n
        real x(n)[*]
        integer i

        x = [(i, i = 1,n)] * this_image()
        sync all
    end subroutine
end
