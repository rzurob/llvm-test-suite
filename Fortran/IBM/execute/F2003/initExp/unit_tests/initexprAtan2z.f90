!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : ATAN2 intrinsic
!*
!* DESCRIPTION                : real type, with signed zero
!* ===================================================================

program main
implicit none
logical precision_r4, precision_r8, precision_r16

real(4) :: a=atan2(-0.0e0, -1.0e0)
real(8) :: b=atan2(-0.0d0, -1.0d0)
real(16) :: c=atan2(-0.0q0, -1.0q0)
real(4) :: a2
real(8) :: b2
real(16) :: c2

a2 = atan2(-0.0e0, -1.0e0)
b2 = atan2(-0.0d0, -1.0d0)
c2 = atan2(-0.0q0, -1.0q0)

if (.not. precision_r4(a, a2)) stop 1
if (.not. precision_r8(b, b2)) stop 2
if (.not. precision_r16(c, c2)) stop 3

call sub()
end

@PROCESS XLF2003(NOSIGNDZEROINTR)
subroutine sub()
implicit none
logical precision_r4, precision_r8, precision_r16

real(4) :: a=atan2(-0.0e0, -1.0e0)
real(8) :: b=atan2(-0.0d0, -1.0d0)
real(16) :: c=atan2(-0.0q0, -1.0q0)
real(4) :: a2
real(8) :: b2
real(16) :: c2

a2 = atan2(-0.0e0, -1.0e0)
b2 = atan2(-0.0d0, -1.0d0)
c2 = atan2(-0.0q0, -1.0q0)

if (.not. precision_r4(a, a2)) stop 4
if (.not. precision_r8(b, b2)) stop 5
if (.not. precision_r16(c, c2)) stop 6
end subroutine
