! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/fxmdvn01.sh fxmdvp27 cxmdvp13
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
!* TEST CASE TITLE              : fxmdvp27.f
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
!*                              : binding label and option -qmixed, use module
!*                              : iso_c_binding, in scope of F sub and C main.
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
real*4  :: bb = 0.0
real*8  :: cc = 0.0D0
real*16 :: dd = 0.0D0
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


subroutine fsub()
use mod

logical precision_r4
logical precision_r8


IF ( .not.precision_r4(a, 2.0) .or. .not.precision_r4(aa, 2.0) ) THEN
    ERROR STOP 51
END IF

IF ( .not.precision_r4(b, 2.0) .or. .not.precision_r4(bb, 2.0) ) THEN
   error stop 52
END IF

IF ( .not.precision_r8(c, 2.0D0) .or. .not.precision_r8(cc, 2.0D0) ) THEN
  ERROR STOP 53
END IF

IF ( .not.precision_r8(d, 2.0D0) .or. .not.precision_r8(dd, 2.0D0) ) THEN
  ERROR STOP 53
END IF

do i = 1, 10
    IF(.not.precision_r4(a1(i), 2.0) .or. &
       .not.precision_r4(b1(i),2.0) .or. &
       .not.precision_r8(c1(i),2.0D0) .or. &
       .not.precision_r8(d1(i),3.0D0)) THEN
      ERROR STOP 54
    END IF
end do
  

do j= 1, 10
   do k = 1, 10 
      IF(.not.precision_r4(a2(k,j),2.0) .or. &
         .not.precision_r4(b2(k,j),2.0) .or. &
         .not.precision_r8(c2(k,j),2.0D0) .or. &
         .not.precision_r8(d2(k,j),3.0D0))THEN
       ERROR STOP 55
      END IF
   end do
end do

do i = 1, 2
   do j = 1, 3
     if ( .not.precision_r4(a3(j,i,1), 2.0) .or. &
          .not.precision_r4(b3(j,i,1), 2.0) .or. &
          .not.precision_r8(c3(j,i,1), 2.0D0) .or. &
          .not.precision_r8(d3(j,i,1), 3.0D0) ) then
        error stop 56

     end if
   end do
end do

a = 0.0
aa = 0.0
b = 0.0
bb = 0.0

c = 0.0D0
cc = 0.0D0
dd = 0.0D0
d = 1.0D0

a1 = 0.0
a2 = 0.0
a3 = 0.0

b1 = 0.0
b2 = 0.0
b3 = 0.0

c1 = 0.0D0
c2 = 0.0D0
c3 = 0.0D0

d1 = 1.0D0
d2 = 1.0D0
d3 = 1.0D0


end 
