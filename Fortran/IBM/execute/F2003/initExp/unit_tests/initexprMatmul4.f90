!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : MATMUL intrinsic
!*
!* DESCRIPTION                : complex type
!* ===================================================================

implicit none
integer :: i

real(4), parameter :: tolerate4 = 5.e-7
real(8), parameter :: tolerate8 = 5.d-16
real(16), parameter :: tolerate16 = 5.q-31

complex(4), dimension(2,3) :: A4
complex(4), dimension(3,2) :: B4

complex(8), dimension(9,10), parameter :: &
  &  A8=reshape((/(cmplx(i,-i),i=111,200)/), (/9,10/))
complex(8), dimension(10,21), parameter :: &
  &  B8=reshape((/(cmplx(-i,i),i=1,210)/), (/10,21/))

complex(16), dimension(7,12), parameter :: &
  &  A16=reshape((/(cmplx(i,i),i=1,84)/), (/7,12/))
complex(16), dimension(12,67), parameter :: &
  &  B16=reshape((/(cmplx(i,i),i=-804,-1)/), (/12,67/))


parameter (A4=reshape((/(1.0_4,1.3_4),(4,4.2),(-2.0,1.9),(-5.5,7.4),(-3,6),(1.2,2.1)/),(/2,3/)))
parameter (B4=reshape((/(7.0_4,8),(9,10),(11,12),(6.22_4,4.402_4), &
  & (11.7496_4,5.31_4),(1.903_4,1.72_4),(5.86_4,9.51_4)/),(/3,2/)))

complex(4), dimension(2,2) :: res4a=matmul(A4, B4), res4b
complex(8), dimension(9,21) :: res8a=matmul(A8, B8), res8b
complex(16), dimension(7,67) :: res2a=matmul(A16, B16), res2b

res2b = matmul(A16,B16)
if (.not. all(complex16Equal(res2a, res2b))) stop 1

res4b = matmul(A4,B4)
if (.not. all(complex4Equal(res4a, res4b))) stop 2

res8b = matmul(A8,B8)
if (.not. all(complex8Equal(res8a, res8b))) stop 3


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

