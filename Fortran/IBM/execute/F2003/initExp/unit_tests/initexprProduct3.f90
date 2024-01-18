!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : PRODUCT intrinsic
!*
!* DESCRIPTION                : complex type
!* ===================================================================

implicit none

integer :: i

real(4), parameter :: tolerate4 = 5.e-7
real(8), parameter :: tolerate8 = 5.d-16
real(16), parameter :: tolerate16 = 5.q-31

complex :: res1=product( (/(2.0,3.0),(3.0,4.0),(4.0,-5.0)/) )
complex, parameter,dimension(5) :: a=(/(-3.,3.),(-7.,2.),(-5.,5.),(2.,-7.),(3.,-3.)/)
complex :: res2=product( a, mask=a.eq.(-2.,2.))

complex, parameter :: b(2,3)=reshape( (/(1.e-4,9.e-5), (1.3e-2,-9.5e-7), &
& (9.6e-2,-2.3e-7), (7.6e-9, 0.0),(-6.99e-7,1.207e-2),(1.67e-9,-5.2153e-2)/), (/2,3/))
complex :: res3(3)=product(b, dim=1), res4(2)=product(b, dim=2)
complex :: res5(2)=product(b, dim=2, mask=b .ne. (2.0,2.0))

complex(8), parameter :: c(2,3,4,5)=reshape((/(i,i=1,120)/), &
  &  (/2,3,4,5/))
complex(8) :: res18(2,3,5)=product(c, dim=3)

complex(16) :: res11=product((/(7.565146Q08,1.7192802Q23),(3.720466Q59,1.5160763q69), &
  & (2.108214q90,1.1142725Q52),(1.1385402q83,0.0q0)/))

if (.not. complex4Equal(res1, product( (/(2.0,3.0),(3.0,4.0),(4.0,-5.0)/)))) error stop 1
if (.not. complex4Equal(res2, product( a, mask=a.eq.(-2.,2.)))) error stop 2
if (.not. all(complex4Equal(res3, product(b, dim=1)))) error stop 4
if (.not. all(complex4Equal(res4, product(b, dim=2)))) error stop 5
if (.not. all(complex4Equal(res5, product(b, dim=2, mask=b .ne. (2.0,2.0))))) error stop 6

if (.not. all(complex8Equal(res18, product(c, dim=3)))) error stop 7

if (.not. complex16Equal(res11, product((/(7.565146Q08,1.7192802Q23),(3.720466Q59,1.5160763q69), &
  & (2.108214q90,1.1142725Q52),(1.1385402q83,0.0q0)/)))) error stop 8

    contains

    logical elemental function complex4Equal (cx1, cx2)
        complex(4), intent(in) :: cx1, cx2

        complex4Equal = (abs(real(cx1,4) - real(cx2,4)) <= &
                abs(real(cx1,4) + real(cx2,4))*tolerate4)

        complex4Equal = complex4Equal .and. &
            (abs(aimag(cx1) - aimag(cx2)) <= abs(aimag(cx1) + aimag(cx2))*tolerate4)
    end function

    logical elemental function complex8Equal (cx1, cx2)
        complex(8), intent(in) :: cx1, cx2

        complex8Equal = (abs(real(cx1,8) - real(cx2,8)) <= &
                abs(real(cx1,8) + real(cx2,8))*tolerate8)

        complex8Equal = complex8Equal .and. &
            (abs(dimag(cx1) - dimag(cx2)) <= abs(dimag(cx1) + dimag(cx2))*tolerate8)
    end function

    logical elemental function complex16Equal (cx1, cx2)
        complex(16), intent(in) :: cx1, cx2

        complex16Equal = (abs(real(cx1,16) - real(cx2,16)) <= &
                abs(real(cx1,16) + real(cx2,16))*tolerate16)

        complex16Equal = complex16Equal .and. &
            (abs(qimag(cx1) - qimag(cx2)) <= abs(qimag(cx1) + qimag(cx2))*tolerate16)
    end function
end
