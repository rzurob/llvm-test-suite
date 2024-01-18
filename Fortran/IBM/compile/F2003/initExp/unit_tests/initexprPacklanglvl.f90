!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : PACK intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

implicit none
integer :: i

integer(1) :: z1(1)=pack((/1_1/), mask=.true.)
integer(2) :: z2(1)=pack((/1_2/), mask=.true.)
integer(4) :: z4(1)=pack((/1_4/), mask=.true.)
integer(8) :: z8(1)=pack((/1_8/), mask=.true.)

logical(1) :: x1(1)=pack((/.true._1/), mask=.true.)
logical(2) :: x2(1)=pack((/.true._2/), mask=.true.)
logical(4) :: x4(1)=pack((/.true._4/), mask=.true.)
logical(8) :: x8(1)=pack((/.true._8/), mask=.true.)

real(4) :: y4(1)=pack((/1._4/), mask=.true.)
real(8) :: y8(1)=pack((/2._8/), mask=.true.)
real(16) :: y16(1)=pack((/3._16/), mask=.true.)

complex(4) :: t4(1)=pack((/(1._4,1.4_4)/), mask=.true.)
complex(8) :: t8(1)=pack((/(2._8,2._8)/), mask=.true.)
complex(16) :: t16(1)=pack((/(3._16,3._16)/), mask=.true.)

character :: c(2)=pack((/'a','b'/), mask=.true.)
end
