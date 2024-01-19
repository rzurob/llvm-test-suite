!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : ANINT intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

real(4) ::  x1=anint(3.555), x2=anint(3.555D0, kind=4)
real(8) :: y=anint(3.555D0)
real(16) :: z=anint(3.555Q0)

end
