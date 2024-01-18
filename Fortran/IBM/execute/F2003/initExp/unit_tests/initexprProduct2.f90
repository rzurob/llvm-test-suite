!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : PRODUCT intrinsic
!*
!* DESCRIPTION                : real type
!* ===================================================================

implicit none

integer :: i

real :: res1=product( (/2,3,4/) )
real, parameter,dimension(5) :: a=(/-3,-7,-5,2,3/)
real :: res2=product( a, mask=a.gt.-5)

real, parameter :: b(2,3)=reshape( (/-2,3,5,-4,7,3/), (/2,3/))
real :: res3(3)=product(b, dim=1), res4(2)=product(b, dim=2)
real :: res5(2)=product(b, dim=2, mask=b .gt. 2)

real(8), parameter :: c(2,3,4,5)=reshape((/(i,i=1,120)/), &
  &  (/2,3,4,5/))
real(8) :: res18(2,3,5)=product(c, dim=3)

real(16) :: res11=product((/1.1_16,9.9_16,86.86_16,9.9_16,75.75_16,5.5_16/))

if (res2 .ne. product( a, mask=a.gt.-5)) stop 3
if (.not. all(res3 .eq. product(b, dim=1))) stop 4
if (.not. all(res4 .eq. product(b, dim=2))) stop 5
if (.not. all(res5 .eq. product(b, dim=2, mask=b .gt. 2))) stop 6

if (.not. all(res18 .eq. product(c, dim=3))) stop 7

if (res11 .ne. product((/1.1_16,9.9_16,86.86_16,9.9_16,75.75_16,5.5_16/))) stop 8

end
