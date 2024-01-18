!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : SUM intrinsic
!*
!* DESCRIPTION                : complex type
!* ===================================================================

implicit none

integer :: i

complex :: res1=sum( (/(2.0,3.0),(3.0,4.0),(4.0,-5.0)/) )
complex, parameter,dimension(5) :: a=(/(-3.,3.),(-7.,2.),(-5.,5.),(2.,-7.),(3.,-3.)/)
complex :: res2=sum( a, mask=a.eq.(-2.,2.))

complex, parameter :: b(2,3)=reshape( (/(1.e-4,9.e-5), (1.3e-2,-9.5e-7), &
& (9.6e-2,-2.3e-7), (7.6e-9, 0.0),(-6.99e-7,1.207e-2),(1.67e-9,-5.2153e-2)/), (/2,3/))
complex :: res3(3)=sum(b, dim=1), res4(2)=sum(b, dim=2)
complex :: res5(2)=sum(b, dim=2, mask=b .ne. (2.0,2.0))

complex(8), parameter :: c(2,3,4,5)=reshape((/(i,i=1,120)/), &
  &  (/2,3,4,5/))
complex(8) :: res18(2,3,5)=sum(c, dim=3)

complex(16) :: res11=sum((/(7.565146Q08,1.7192802Q23),(3.720466Q59,1.5160763q69), &
  & (2.108214q90,1.1142725Q52),(1.1385402q83,0.0q0)/))

if (res1 .ne. sum( (/(2.0,3.0),(3.0,4.0),(4.0,-5.0)/) )) stop 1
if (res2 .ne. sum( a, mask=a.eq.(-2.,2.))) stop 2
if (.not. all(res3 .eq. sum(b, dim=1))) stop 4
if (.not. all(res4 .eq. sum(b, dim=2))) stop 5
if (.not. all(res5 .eq. sum(b, dim=2, mask=b .ne. (2.0,2.0)))) stop 6

if (.not. all(res18 .eq. sum(c, dim=3))) stop 7

if (res11 .ne. sum((/(7.565146Q08,1.7192802Q23),(3.720466Q59,1.5160763q69), &
  & (2.108214q90,1.1142725Q52),(1.1385402q83,0.0q0)/))) stop 8

end
