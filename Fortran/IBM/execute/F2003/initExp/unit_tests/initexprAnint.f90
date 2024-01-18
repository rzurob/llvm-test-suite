!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : ANINT intrinsic
!*
!* DESCRIPTION                : real type
!* ===================================================================

logical precision_r4, precision_r8, precision_r16

real(4) ::  x1=anint(3.555), x2=anint(3.555D0, kind=4)
real(8) :: y=anint(3.555D0)
real(16) :: z=anint(3.555Q0)

if (.not. precision_r4(x1, anint(3.555))) stop 1
if (.not. precision_r4(x2, anint(3.555D0,kind=4))) stop 2
if (.not. precision_r8(y, anint(3.555D0))) stop 3
if (.not. precision_r16(z, anint(3.555Q0))) stop 4
end
