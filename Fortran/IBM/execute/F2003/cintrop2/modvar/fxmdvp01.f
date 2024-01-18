! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/fxmdvn01.sh fxmdvp01 cxmdvp01
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
!*                              : variable with diffrent kind parameter, in the
!*                              : scope of Fortram sub which called from C main.
!*                              : Test case tests the scalar and array of 1, 2
!*                              : and 3 dimension bind c module variable of
!*                              : real, real4, real8. The value of variables
!*                              : are initialized in C main(), then call Fortran
!*                              : subroutine. In Fortran subroutine, the value
!*                              : of variables are checked. If is not equal to
!*                              : the initialized values in C, test case exit
!*                              : with non_zero number. Then, Fortran subroutine
!*                              : change the value of the variables and return
!*                              : to C main(). After that, values of the
!*                              : variables are checked in C main() to verify
!*                              : value have been changed in C subroutine. The
!*                              : C subroutine is complied by xlc under AIX,
!*                              : and both xlc and gcc under MACOS, SLES and
!*                              : REDHAT.
!234567890123456789012345678901234567890123456789012345678901234567890

module mod

bind(c) :: a, c
real :: a = 0.0
real(4) :: b = 0.0
real(8) :: c = 0.0D0
real(16) :: d = 0.0Q0
bind(c) :: b, d

real*4 :: bb = 0.0
real*8 :: cc = 0.0D0
real*16 :: dd = 0.0Q0
bind(c) :: bb, cc, dd

real, dimension(10), bind(c) :: a1
real, dimension(10, 10), bind(c) :: a2
real, bind(c) :: a3(3, 2, 1)

real(4), dimension(10), bind(c) :: b1
real(4), dimension(10, 10), bind(c) :: b2
real(4), bind(c) :: b3(3, 2, 1)

real(8), dimension(10), bind(c) :: c1
real(16), dimension(10), bind(c) :: d1
real(8), dimension(10, 10), bind(c) :: c2
real(16), dimension(10, 10), bind(c) :: d2
real(8), bind(c) :: c3(3, 2, 1)
real(16), bind(c) :: d3(3, 2, 1)

end module

subroutine fsub()
use mod

logical precision_r4
logical precision_r8
logical precision_r16


IF ( .not.precision_r4(a, 2.0) ) THEN
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

IF ( .not.precision_r16(d, 2.0Q0) .or. &
     .not.precision_r16(dd, 2.0Q0) ) THEN
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

a = 0.0
b = 0.0
bb = 0.0

c = 0.0D0
d = 0.0D0
cc = 0.0D0
dd = 0.0Q0

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


end
