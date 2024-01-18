! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/fxmdvn04.sh fxmdvp16 cxmdvp16
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
!* TEST CASE TITLE              : fxmdvp16.f
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
!* REQUIRED COMPILER OPTIONS    : -qfree=f90 -qmixed
!*
!* DESCRIPTION                  : Test the interoperability of complex module
!*                              : variable with diffrent kind with -qmixed and
!*                              : binding label, in scope of c main calling 
!*                              : fortran subroutine.
!234567890123456789012345678901234567890123456789012345678901234567890

module mod
character(*), parameter :: kk = "  Comp1ex  "

bind(c, name = "A") :: a
bind(c, name = "  c  ") :: c
complex :: a = (0.0, 1.0)
complex(4) :: b = (0.0, 1.0)
complex(8) :: c = (0.0D0, 1.0D0)
complex(16) :: d = (0.0Q0, 1.0Q0)
bind(c, name = "a") :: b
bind(c, name = kk(1:3)) :: d

complex*8 :: bb = (0.0,1.0)
complex*16 :: cc = (0.0D0,1.0D0)
complex*32 :: dd = (0.0Q0,1.0Q0)
bind(c, name = "  BB  b  "(3:4)) :: bb
bind(c, name = kk) :: cc
bind(c, name = trim("  bb  ")) :: dd

complex, dimension(10), bind(c, name = " A"// kk(7:7)) :: a1
complex, dimension(10, 10), bind(c, name = " a1 ") :: a2
complex, bind(c, name = "A3") :: a3(3, 2, 1)

complex(4), dimension(10), bind(c, name = "B1") :: b1
complex(4), dimension(10, 10), bind(c, name = "b2") :: b2
complex(4), bind(c, name = "b1") :: b3(3, 2, 1)

complex(8), dimension(10), bind(c, name = "  c1  ") :: c1
complex(16), dimension(10), bind(c, name = "d1     ") :: d1
complex(8), dimension(10, 10) :: c2
complex(16), dimension(10, 10) :: d2
bind(c, name = "C2   "(1:4)) :: c2
bind(c, name = "    d2        "(1:10)) :: d2
complex(8), bind(c, name="c3") :: c3(3, 2, 1)
complex(16), bind(c, name="C3") :: d3(3, 2, 1)

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

a  = (0.0,1.0)
b  = (0.0,1.0)
bb = (0.0,1.0)

c = (0.0D0,1.0D0)
d = (0.0D0,1.0D0)
cc = (0.0D0,1.0D0)
dd = (0.0Q0,1.0Q0)  
  
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
