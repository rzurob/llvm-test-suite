!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : EOSHIFT intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

implicit none

integer(1) :: i1(1)=eoshift((/1_1/),shift=1)
integer(2) :: i2(1)=eoshift((/1_2/),shift=1)
integer(4) :: i4(1)=eoshift((/1_4/),shift=1)
integer(8) :: i8(1)=eoshift((/1_8/),shift=1)

real(4) :: r4(1)=eoshift((/1.0_4/),shift=-1)
real(8) :: r8(1)=eoshift((/1.0_8/),shift=-1)
real(16) :: r16(1)=eoshift((/1.0_16/),shift=-1)

complex(4) :: c4(1)=eoshift((/(1.0_4,-1.0_4)/),shift=-1)
complex(8) :: c8(1)=eoshift((/(1.0_8,-1.0_8)/),shift=-1)
complex(16) :: c16(1)=eoshift((/(1.0_16,-1.0_16)/),shift=-1)

character :: cc(1)=eoshift((/'Z'/), shift=0)
end
