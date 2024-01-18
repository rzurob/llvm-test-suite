!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : SUM intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

implicit none

integer :: i

integer(1) :: i1=sum((/1_1,9_1/))
integer(2) :: i2=sum((/1_2,9_2/))
integer(4) :: i4=sum((/1_4,9_4/))
integer(8) :: i8=sum((/1_8,9_8/))

real(4) :: r4=sum((/1.0_4,9.1_4/))
real(8) :: r8=sum((/1.9_8,9.1_8/))
real(16) :: r16=sum((/1.9_16,9.1_16/))

complex(4) :: c4=sum((/(1.0_4,9.1_4)/))
complex(8) :: c8=sum((/(1.9_8,9.1_8)/))
complex(16) :: c16=sum((/(1.9_16,9.1_16)/))

end
