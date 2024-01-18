!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : ANINT intrinsic
!*
!* DESCRIPTION                : real type
!* ===================================================================

logical precision_r4, precision_r8, precision_r16

real(4) ::  x1=anint(3.555), x2=anint(3.555D0, kind=4)
real(8) :: y=anint(3.555D0)
real(16) :: z=anint(3.555Q0)

if (.not. precision_r4(x1, anint(3.555))) error stop 1
if (.not. precision_r4(x2, anint(3.555D0,kind=4))) error stop 2
if (.not. precision_r8(y, anint(3.555D0))) error stop 3
if (.not. precision_r16(z, anint(3.555Q0))) error stop 4
end
