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
!* DESCRIPTION                  : Test the interoperability of complex module
!*                              : variable with diffrent kind parameter, use
!*                              : module iso_c_binding. The variable is used
!*                              : in fortran subroutine and called by C main.
!234567890123456789012345678901234567890123456789012345678901234567890

module mod
use iso_c_binding

complex :: a = (0.0, 1.0)
complex(c_float_complex) :: b = (0.0, 1.0)
complex(c_double_complex) :: c = (0.0D0, 1.0D0)
complex(c_long_double_complex) :: d = (0.0Q0,1.0Q0)
bind(c) :: a, b, c, d

complex(c_long_double_complex) :: dd = (0.0Q0,1.0Q0)
bind(c) :: dd

complex*8 :: bb = (0.0,1.0)
complex*16 :: cc = (0.0D0,1.0D0)
bind(c) :: bb, cc

complex, dimension(10), bind(c) :: a1
complex, dimension(10, 10), bind(c) :: a2
complex, bind(c) :: a3(3, 2, 1)

complex(c_float_complex), dimension(10), bind(c) :: b1
complex(c_float_complex), dimension(10, 10), bind(c) :: b2
complex(c_float_complex), bind(c) :: b3(3, 2, 1)

complex(c_double_complex), dimension(10), bind(c) :: c1
complex(c_long_double_complex), dimension(10), bind(c) :: d1
complex(c_double_complex), dimension(10, 10), bind(c) :: c2
complex(c_long_double_complex), dimension(10,10), bind(c) :: d2
complex(c_double_complex), bind(c) :: c3(3, 2, 1)
complex(c_long_double_complex), bind(c) :: d3(3, 2, 1)

end module


subroutine fsub()
use mod

logical precision_x8
logical precision_x16
logical precision_x32


IF ( .not.precision_x8(a,(1.0, 3.0)) ) THEN
    ERROR STOP 51
END IF

IF ( .not.precision_x8(b,(1.0, 3.0)) .or. &
     .not.precision_x8(bb,(1.0,3.0))) THEN
   error stop 52
END IF

IF(.not.precision_x16(c,(1.0D0,3.0D0)) .or. &
   .not.precision_x16(cc,(1.0D0,3.0D0))) THEN
  ERROR STOP 53
END IF

IF ( .not.precision_x32(d,(1.0Q0,3.0Q0)) &
     .or. .not.precision_x32(dd,(1.0Q0,3.0Q0))) THEN
  ERROR STOP 53
END IF

do i = 1, 10
  IF(.not.precision_x8(a1(i),(1.0,3.0)) .or. &
     .not.precision_x8(b1(i),(1.0,3.0))&
     .or. .not.precision_x16(c1(i),(1.0D0,3.0D0)) .or. &
     .not.precision_x32(d1(i),(1.0Q0,3.0Q0)) ) THEN
       ERROR STOP 54
  END IF
end do


do j= 1, 10
   do i = 1, 10
     IF ( .not.precision_x8(a2(i,j), (1.0,3.0)) .or. &
          .not.precision_x8(b2(i,j), (1.0,3.0)) .or. &
          .not.precision_x16(c2(i,j),(1.0D0,3.0D0)) .or. &
          .not.precision_x32(d2(i,j),(1.0Q0,3.0Q0)) ) THEN
       ERROR STOP 55
     END IF
   end do
end do

do i = 1, 2
   do j = 1, 3
     if ( .not.precision_x8(a3(j,i,1),(1.0,3.0)) .or. &
          .not.precision_x8(b3(j,i,1),(1.0,3.0)) .or. &
          .not.precision_x16(c3(j,i,1),(1.0D0,3.0D0)) .or. &
          .not.precision_x32(d3(j,i,1),(1.0Q0,3.0Q0)) ) then
        error stop 56
     end if
   end do
end do

a = (0.0,1.0)
b = (0.0,1.0)
bb = (0.0,1.0)

c = (0.0D0,1.0D0)
cc = (0.0D0,1.0D0)
dd = (0.0Q0,1.0Q0)
d = (0.0Q0,1.0Q0)

a1 = (0.0,1.0)
a2 = (0.0,1.0)
a3 = (0.0,1.0)

b1 = (0.0,1.0)
b2 = (0.0,1.0)
b3 = (0.0,1.0)

c1 = (0.0D0,1.0D0)
c2 = (0.0D0,1.0D0)
c3 = (0.0D0,1.0D0)

d1 = (0.0Q0,1.0Q0)
d2 = (0.0Q0,1.0Q0)
d3 = (0.0Q0,1.0Q0)


end
