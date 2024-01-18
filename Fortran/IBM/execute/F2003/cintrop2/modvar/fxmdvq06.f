! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/fxmdvn06.sh fxmdvq06 cxmdvq06
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
!* DESCRIPTION                  : Test the interoperability of complex16 module
!*                              : variable with corresponding c variable
!*                              : compiled with -qlongdouble. Use the variables
!*                              : in fmain and csub which calls fsub.
!234567890123456789012345678901234567890123456789012345678901234567890

module mod

complex(16) :: a = (0.0Q0, 1.0Q0)
bind(c) :: a


complex*32, bind(c) :: aa = (0.0Q0,1.0Q0)


complex(16), dimension(10), bind(c) :: a1
complex(16), dimension(10, 10), bind(c) :: a2
complex(16), bind(c) :: a3(3, 2, 1)

end module

use mod


logical precision_x32


if(.not.precision_x32(a,(0.0Q0,1.0Q0)) &
   .or. &
   .not.precision_x32(aa,(0.0Q0,1.0Q0)) ) then
    error stop 50
end if

a1 = (0.0Q0,1.0Q0)
a2 = (0.0Q0,1.0Q0)
a3 = (0.0Q0,1.0Q0)


call csub()

IF ( .not.precision_x32(a,(1.0Q0, 3.0Q0)) ) THEN
    ERROR STOP 51
END IF

IF ( .not.precision_x32(aa,(1.0Q0, 3.0Q0)) ) THEN
   error stop 52
END IF


do i = 1, 10
  IF(.not.precision_x32(a1(i),(1.0Q0,3.0Q0))  ) THEN
      ERROR STOP 54
  END IF
end do


do j= 1, 10
   do i = 1, 10
     IF ( .not.precision_x32(a2(i,j), (1.0Q0,3.0Q0))  ) THEN
       ERROR STOP 55
     END IF
   end do
end do

do i = 1, 2
   do j = 1, 3
     if ( .not.precision_x32(a3(j,i,1),(1.0Q0,3.0Q0))  ) then
        error stop 56
     end if
   end do
end do

end

subroutine fsub()
use mod

a = a + (2.0Q0,1.0Q0)
aa = aa + (2.0Q0,1.0Q0)
a1 = a1 + (2.0Q0,1.0Q0)
a2 = a2 + (2.0Q0,1.0Q0)
a3 = a3 + (2.0Q0,1.0Q0)

end

