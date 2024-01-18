! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/fxmdvn01.sh fxmdvq27 cxmdvq13
! %COMPOPTS: -qfree=f90 -qmixed
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         INBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : fxmdvq27.f
!
!* PROGRAMMER                   : Yubin Liao
!* DATE                         : Sep. 24, 2003
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf90
!* REQUIRED COMPILER OPTIONS    : -qfree=f90 -qmiexed
!*
!* DESCRIPTION                  : Test the interoperability of real module
!*                              : variable with diffrent kind parameter with
!*                              : binding label and option -qmixed. Use the
!*                              : variables in fmain and csub which calls fsub.
!*                              : use module iso_c_binding
!234567890123456789012345678901234567890123456789012345678901234567890

module mod
use iso_c_binding

real :: a = 0.0
real(c_float) :: b = 0.0
real(c_double) :: c = 0.0D0
real(c_long_double) :: d = 0.0D0
bind(c, name = "d") :: a
bind(c, name = "c") :: b
bind(c, name = "a") :: c
bind(c, name = "b") :: d

real*4, bind(c, name = "A") :: aa = 0.0
real*4 :: bb = 0.0
real*8 :: cc = 0.0D0
real*16 :: dd = 0.0Q0
bind(c, name = "B") :: bb
bind(c, name = "C") :: cc
bind(c, name = "D") :: dd

real, dimension(10), bind(c, name = "a2") :: a1
real, dimension(10, 10), bind(c, name = "a3") :: a2
real, bind(c, name = "a1") :: a3(3, 2, 1)

real(c_float), dimension(10), bind(c, name = "A2") :: b1
real(c_float), dimension(10, 10), bind(c, name = "A1") :: b2
real(c_float), bind(c, name = "A3") :: b3(3, 2, 1)

real(c_double), dimension(10), bind(c, name = "CCC") :: c1
real(c_long_double), dimension(10), bind(c, name = "CCc") :: d1
real(c_double), dimension(10, 10), bind(c, name = "CcC") :: c2
real(c_long_double), dimension(10,10), bind(c, name = " Ccc ") :: d2
real(c_double), bind(c, name = "cCC") :: c3(3, 2, 1)
real(c_long_double), bind(c, name = "cCc") :: d3(3, 2, 1)

end module

use mod

logical precision_r4
logical precision_r8
logical precision_r16

! check the initialization value in the moudle.
if ( .not.precision_r4(a, 0.0) .or. &
     .not.precision_r4(aa, 0.0) .or. &
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

d1 = 1.0Q0
d2 = 1.0Q0
d3 = 1.0Q0

call csub()


IF ( .not.precision_r4(a, 2.0) .or. .not.precision_r4(aa, 2.0) ) THEN
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
       .not.precision_r16(d1(i),3.0Q0)) THEN
      ERROR STOP 54
    END IF
end do
  

do j= 1, 10
   do k = 1, 10 
      IF(.not.precision_r4(a2(k,j),2.0) .or. &
         .not.precision_r4(b2(k,j),2.0) .or. &
         .not.precision_r8(c2(k,j),2.0D0) .or. &
        .not.precision_r16(d2(k,j),3.0Q0))THEN
       ERROR STOP 55
      END IF
   end do
end do

do i = 1, 2
    do j = 1, 3
     if ( .not.precision_r4(a3(j,i,1), 2.0) .or. &
          .not.precision_r4(b3(j,i,1), 2.0) .or. &
          .not.precision_r8(c3(j,i,1), 2.0D0) .or. &
          .not.precision_r16(d3(j,i,1), 3.0Q0) ) then
        error stop 56

     end if
    end do
end do

end 

subroutine fsub()
use mod

aa = aa + 1.0
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
