! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/fxmdvt01.sh fxmdvt01 cxmdvn13
! %COMPOPTS: -qfree=f90 -qmixed -qextchk
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
!* REQUIRED COMPILER OPTIONS    : -qfree=f90 -qmiexed
!*
!* DESCRIPTION                  : Test the interoperability of real module
!*                              : variable with diffrent kind parameter with
!*                              : binding label and option -qmixed
!234567890123456789012345678901234567890123456789012345678901234567890

module mod

bind(c, name = "d") :: a
bind(c, name = "a") :: c
real :: a = 0.0
real(4) :: b = 0.0
real(8) :: c = 0.0D0

#if  (!defined(SUSE10_UP))
	real(8) :: d = 0.0D0
#else
	real(16) :: d = 0.0D0
#endif

bind(c, name = "c") :: b
bind(c, name = "b") :: d

real*4, bind(c, name = "A") :: aa = 0.0
real*4 :: bb = 0.0
real*8 :: cc = 0.0D0

#if  (!defined(SUSE10_UP))
	real*8 :: dd = 0.0D0
#else
	real*16 :: dd = 0.0D0
#endif

bind(c, name = "B") :: bb
bind(c, name = "C") :: cc
bind(c, name = "D") :: dd

real, dimension(10), bind(c, name = "a2") :: a1
real, dimension(10, 10), bind(c, name = "a3") :: a2
real, bind(c, name = "a1") :: a3(3, 2, 1)

real(4), dimension(10), bind(c, name = "A2") :: b1
real(4), dimension(10, 10), bind(c, name = "A1") :: b2
real(4), bind(c, name = "A3") :: b3(3, 2, 1)

real(8), dimension(10), bind(c, name = "CCC") :: c1

#if  (!defined(SUSE10_UP))
	real(8), dimension(10), bind(c, name = "CCc") :: d1
#else
	real(16), dimension(10), bind(c, name = "CCc") :: d1
#endif

real(8), dimension(10, 10), bind(c, name = "CcC") :: c2

#if  (!defined(SUSE10_UP))
	real(8), dimension(10,10), bind(c, name = " Ccc ") :: d2
#else
	real(16), dimension(10,10), bind(c, name = " Ccc ") :: d2
#endif

real(8), bind(c, name = "cCC") :: c3(3, 2, 1)

#if  (!defined(SUSE10_UP))
	real(8), bind(c, name = "cCc") :: d3(3, 2, 1)
#else
	real(16), bind(c, name = "cCc") :: d3(3, 2, 1)
#endif

end module

use mod

logical precision_r4
logical precision_r8

! check the initialization value in the moudle.
if ( .not.precision_r4(a, 0.0) .or. &
     .not.precision_r4(aa, 0.0) .or. &
     .not.precision_r4(b, 0.0) .or. &
     .not.precision_r4(bb, 0.0) .or. &
     .not.precision_r8(c, 0.0D0) .or. &
     .not.precision_r8(d, 0.0D0) .or. &
     .not.precision_r8(cc, 0.0D0) .or. &
     .not.precision_r8(dd, 0.0D0) ) then
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

d1 = 1.0D0
d2 = 1.0D0
d3 = 1.0D0

call csub()


IF ( .not.precision_r4(a, 2.0) .or. &
     .not.precision_r4(aa, 2.0) ) THEN
    ERROR STOP 51
END IF

IF ( .not.precision_r4(b, 2.0) .or. &
     .not.precision_r4(bb, 2.0) ) THEN
   error stop 52
END IF

IF ( .not.precision_r8(c, 2.0D0) .or. &
     .not.precision_r8(cc, 2.0D0) ) THEN
  ERROR STOP 53
END IF

IF ( .not.precision_r8(d, 2.0D0) .or. &
     .not.precision_r8(dd, 2.0D0) ) THEN
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
         .not.precision_r8(d2(k,j),3.0D0))  THEN
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

end
