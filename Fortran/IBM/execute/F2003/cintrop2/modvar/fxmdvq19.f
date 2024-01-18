! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/fxmdvn01.sh fxmdvq19 cxmdvq01
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : Sep. 24, 2003
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    : -qfree=f90
!*
!* DESCRIPTION                  : Test the interoperability of real module
!*                              : variable with diffrent kind parameter.
!*                              : in the scope of f main call csug call fsub.
!*                              : Useing module iso_c_binding.
!234567890123456789012345678901234567890123456789012345678901234567890

module mod
use iso_c_binding
real :: a = 0.0
real(c_float) :: b = 0.0
real(c_double) :: c = 0.0D0
real(c_long_double) :: d = 0.0D0
bind(c) :: a, b, c, d

real*4 :: bb = 0.0
real*8 :: cc = 0.0D0
real*16 :: dd = 0.0Q0
bind(c) :: bb, cc, dd

real, dimension(10), bind(c) :: a1
real, dimension(10, 10), bind(c) :: a2
real, bind(c) :: a3(3, 2, 1)

real(c_float), dimension(10), bind(c) :: b1
real(c_float), dimension(10, 10), bind(c) :: b2
real(c_float), bind(c) :: b3(3, 2, 1)

real(c_double), dimension(10), bind(c) :: c1
real(c_long_double), dimension(10), bind(c) :: d1
real(c_double), dimension(10, 10), bind(c) :: c2
real(c_long_double), dimension(10,10), bind(c) :: d2
real(c_double), bind(c) :: c3(3, 2, 1)
real(c_long_double), bind(c) :: d3(3, 2, 1)

end module

use mod

logical precision_r4
logical precision_r8
logical precision_r16

! check the initialization value in the moudle.
if ( .not.precision_r4(a, 0.0) .or. &
     .not.precision_r4(b, 0.0) .or. &
     .not.precision_r4(bb, 0.0) .or. &
     .not.precision_r8(c, 0.0D0) .or. &
     .not.precision_r16(d, 0.0Q0) .or. &
     .not.precision_r8(cc, 0.0D0) .or. &
     .not.precision_r16(dd, 0.0Q0) ) then
    error stop 50
end if

a1 = 0.0
a2 = 0.0
a3 = 0.0

b1 = 0.0
b2 = 0.0
b3 = 0.0

c1 = 0.0D0
c2 = 0.0D0
c3 = 0.0D0

d1 = 0.0Q0
d2 = 0.0Q0
d3 = 0.0Q0

call csub()


IF ( .not.precision_r4(a, 2.0) ) THEN
    ERROR STOP 51
END IF

IF ( .not.precision_r4(b, 2.0) .or. .not.precision_r4(bb, 2.0) ) THEN
   error stop 52
END IF

IF ( .not.precision_r8(c, 2.0D0) .or. .not.precision_r8(cc, 2.0D0) ) THEN
  ERROR STOP 53
END IF

IF ( .not.precision_r16(d, 2.0Q0) .or. .not.precision_r16(dd, 2.0Q0) ) THEN
  ERROR STOP 53
END IF

do i = 1, 10
    IF(.not.precision_r4(a1(i), 2.0) .or. &
       .not.precision_r4(b1(i),2.0) .or. &
       .not.precision_r8(c1(i),2.0D0) .or. &
       .not.precision_r16(d1(i),2.0Q0)) THEN
      ERROR STOP 54
    END IF
end do


do j= 1, 10
   do k = 1, 10
      IF(.not.precision_r4(a2(k,j),2.0) .or. &
         .not.precision_r4(b2(k,j),2.0) .or. &
         .not.precision_r8(c2(k,j),2.0D0) .or. &
         .not.precision_r16(d2(k,j),2.0Q0))THEN
       ERROR STOP 55
      END IF
   end do
end do

do i = 1, 2
    do j = 1, 3
     if ( .not.precision_r4(a3(j,i,1), 2.0) .or. &
          .not.precision_r4(b3(j,i,1), 2.0) .or. &
          .not.precision_r8(c3(j,i,1), 2.0D0) .or. &
          .not.precision_r16(d3(j,i,1), 2.0Q0) ) then
        error stop 56

     end if
    end do
end do

end

subroutine fsub()
use mod
a = 1.0 + a
b = 1.0 + b
bb = 1.0 + bb

c = 1.0D0 + c
cc = 1.0D0 + cc
d = 1.0Q0 + d
dd = 1.0Q0 + dd

a1 = 1.0 + a1
a2 = 1.0 + a2
a3 = 1.0 + a3

b1 = 1.0 + b1
b2 = 1.0 + b2
b3 = 1.0 + b3

c1 = 1.0D0 + c1
c2 = 1.0D0 + c2
c3 = 1.0D0 + c3

d1 = 1.0Q0 + d1
d2 = 1.0Q0 + d2
d3 = 1.0Q0 + d3

end

