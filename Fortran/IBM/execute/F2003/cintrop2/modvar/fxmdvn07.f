! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : Sep. 24, 2003
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    : -qfree=f90
!*
!* DESCRIPTION                  : Test the interoperability of real module
!*                              : variable with diffrent kind parameter with
!*                              : binding label
!*                              : Similar to fxmdvn01.f but with binding labels.
!234567890123456789012345678901234567890123456789012345678901234567890

module mod

character(len = 8), parameter :: CH = "abcdABCD"
bind(c, name='a') :: w
real :: w = 0.0
real(4), bind(c, name = "B") :: x = 0.0
real(8), bind(c, name = "c ") :: y = 0.0D0
real(16), bind(c, name = " D ") :: z = 0.0Q0

real*4 :: ww = 0.0
real*4 :: xx = 0.0
bind(c, name=CH(1:2)) :: ww
bind(c, name=CH(4:5)) :: xx

real*8, bind(c, name = " cc " ) :: yy = 0.0D0
real*16, bind(c, name = " DD ") :: zz = 0.0Q0


real, dimension(10), bind(c, name = "a1") :: w1
real, dimension(10, 10), bind(c, name = " a2 ") :: w2
real, bind(c, name = "    a3   ") :: w3(3, 2, 1)

real(4), dimension(10), bind(c, name = "b1") :: x1
real(4), dimension(10, 10), bind(c, name = "B2   ") :: x2
real(4), bind(c, name = "b3") :: x3(3, 2, 1)

real(8), dimension(10), bind(c, name = "c1") :: y1
real(16), dimension(10), bind(c, name = "d1") :: z1
real(8), dimension(10, 10) :: y2
real(16), dimension(10, 10) :: z2
bind(c, name = "c2 ") :: y2
bind(c, name = "d2 ") :: z2
real(8) :: y3(3, 2, 1)
real(16) :: z3(3, 2, 1)
bind(c, name = "D3") :: z3
bind(c, name = "  c3") :: y3

end module

use mod

logical precision_r4
logical precision_r8
logical precision_r16

! check the initialization value in the moudle.
if ( .not.precision_r4(w, 0.0) .or. &
     .not.precision_r4(x, 0.0) .or. &
     .not.precision_r4(xx, 0.0) .or. &
     .not.precision_r8(y, 0.0D0) .or. &
     .not.precision_r16(z, 0.0Q0) .or. &
     .not.precision_r8(yy, 0.0D0) .or. &
     .not.precision_r16(zz, 0.0Q0) ) then
    error stop 50
end if

w1 = 0.0
w2 = 0.0
w3 = 0.0

x1 = 0.0
x2 = 0.0
x3 = 0.0

y1 = 0.0D0
y2 = 0.0D0
y3 = 0.0D0

z1 = 0.0Q0
z2 = 0.0Q0
z3 = 0.0Q0

call csub()

!check the values after C subroutine call.
IF ( .not.precision_r4(w, 2.0) ) THEN
    ERROR STOP 51
END IF

IF ( .not.precision_r4(x, 2.0) .or. &
     .not.precision_r4(xx, 2.0) ) THEN
   error stop 52
END IF

IF ( .not.precision_r8(y, 2.0D0) .or. &
     .not.precision_r8(yy, 2.0D0) ) THEN
  ERROR STOP 53
END IF

IF ( .not.precision_r16(z, 2.0Q0) .or. &
     .not.precision_r16(zz, 2.0Q0) ) THEN
  ERROR STOP 53
END IF

do i = 1, 10
    IF(.not.precision_r4(w1(i), 2.0) .or. &
       .not.precision_r4(x1(i),2.0) .or. &
       .not.precision_r8(y1(i),2.0D0) .or. &
       .not.precision_r16(z1(i),2.0Q0)) THEN
      ERROR STOP 54
    END IF
end do


do j= 1, 10
  do k = 1, 10
     IF(.not.precision_r4(w2(k,j),2.0) .or. &
        .not.precision_r4(x2(k,j),2.0) .or. &
        .not.precision_r8(y2(k,j),2.0D0) .or. &
        .not.precision_r16(z2(k,j),2.0Q0))  THEN
          ERROR STOP 55
     END IF
  end do
end do

do i = 1, 2
   do j = 1, 3
     if ( .not.precision_r4(w3(j,i,1), 2.0) .or. &
          .not.precision_r4(x3(j,i,1), 2.0) .or. &
          .not.precision_r8(y3(j,i,1), 2.0D0) .or. &
          .not.precision_r16(z3(j,i,1), 2.0Q0) ) then
        error stop 56

     end if
   end do
end do

end