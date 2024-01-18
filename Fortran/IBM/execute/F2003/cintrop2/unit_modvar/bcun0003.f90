! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : May. 24, 2003
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    : -qfree=f90
!*
!* DESCRIPTION                  : Test bind(c) variables work as
!*                              : global variables.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
character(1) ch
bind(c) ch
!complex(4), bind(c) :: cm
real, bind(c) :: cf
integer :: ci, ca(3,2,1)
bind(c) :: ci, ca
end module

subroutine fsub()
use mod
print *, "In Fortran before changing:"
print *, ch
!print *, cm
print *, cf
print *, ci
print *, ca
ch = 'F'
!cm = cm + 1
cf = cf + 1
ci = ci + 1
ca = ca + 1
print *, "In Fortran after changing:"
print *, ch
!print *, cm
print *, cf
print *, ci
print *, ca
end subroutine
