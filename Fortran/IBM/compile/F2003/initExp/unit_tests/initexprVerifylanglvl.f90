!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : VERIFY intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

@process INTSIZE(2)
program main
integer :: c=verify('abcdef', 'de')
end program

@process INTSIZE(4)
subroutine sub1()
integer :: b=verify('april', 'l')
end subroutine

@process INTSIZE(8)
subroutine sub2()
integer :: d=verify('feedback', 'db')
end subroutine
