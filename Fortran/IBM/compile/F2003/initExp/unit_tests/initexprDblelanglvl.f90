!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : DBLE
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

integer(1), parameter :: i1=4
integer(2), parameter :: i2=24
integer(4), parameter :: i4=124
integer(8), parameter :: i8=4321578

double precision :: ri1=dble(i1), ri2=dble(i2), ri4=dble(i4), ri8=dble(i8)

real(4) , parameter :: r4=27.7815802
real(8) , parameter :: r8=984732.837d0
real(16) , parameter :: r16=3.5156214q-3

double precision :: rr4=dble(r4), rr8=dble(r8), rr16=dble(r16)

complex(4), parameter :: c4=(6.96320677,16.64037287)
complex(8), parameter :: c8=(200.6463578,62.2731032)
complex(16), parameter :: c16=(5.21,1.59)

double precision :: rc4=dble(c4), rc8=dble(c8), rc16=dble(c16)

end
