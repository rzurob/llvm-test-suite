!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : SCAN intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

@process INTSIZE(2)
program main
integer :: c=scan('abcdef', 'de')
end program

@process INTSIZE(4)
subroutine sub1()
integer :: b=scan('april', 'l')
end subroutine

@process INTSIZE(8)
subroutine sub2()
integer :: d=scan('feedback', 'db')
end subroutine
