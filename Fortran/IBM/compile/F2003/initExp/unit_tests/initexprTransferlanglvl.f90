!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : TRANSFER intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

implicit none

integer :: i

integer(1) :: i1=transfer(1_1,9_1)
integer(2) :: i2=transfer(1_2,9_2)
integer(4) :: i4=transfer(1_4,9_4)
integer(8) :: i8=transfer(1_8,9_8)

real(4) :: r4=transfer(1.0_4,9.1_4)
real(8) :: r8=transfer(1.9_8,9.1_8)
real(16) :: r16=transfer(1.9_16,9.1_16)

complex(4) :: c4=transfer((1.0_4,9.1_4), (0.0_4,0.0_4))
complex(8) :: c8=transfer((1.9_8,9.1_8), (0.0_8,0.0_8))
complex(16) :: c16=transfer((1.9_16,9.1_16), (0.0_16,0.0_16))

end
