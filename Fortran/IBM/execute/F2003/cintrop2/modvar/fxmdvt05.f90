! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/fxmdvt05.sh fxmdvt05 cxmdvn17
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
!* XL Fortran Test Case                         INBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : fxmdvn17.f
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
!* REQUIRED COMPILER OPTIONS    : -qfree=f90
!*
!* DESCRIPTION                  : Test the interoperability of real16 module
!*                              : variable with with correspoding c variable
!*                              : compiled with -qlongdoube, with binding label
!*                              : and with -qmixed.
!234567890123456789012345678901234567890123456789012345678901234567890

module mod

character(*), parameter :: kk = "  real16  "

real(16) :: a = 0.0Q0
bind(c, name = kk(6:8)) :: a

real*16, bind(c, name = "a") :: A = 0.0Q0

real(16), dimension(10), bind(c, name = "   A1  ") :: a1
real(16), dimension(10, 10), bind(c, name = "   a1  ") :: a2
real(16), bind(c, name = "   "//"A"//"  ") :: a3(3, 2, 1)

complex(16) :: b = (0.0Q0, 1.0Q0)
bind(c) :: b

end module

use mod

logical precision_r16


if ( .not.precision_r16(a, 0.0Q0) .or. &
     .not.precision_r16(A, 0.0Q0) ) then
    error stop 50
end if

a1 = 0.0Q0
a2 = 0.0Q0
a3 = 0.0Q0

call csub()


IF ( .not.precision_r16(a, 2.0Q0) ) THEN
    ERROR STOP 51
END IF

IF ( .not.precision_r16(A, 2.0Q0)  ) THEN
   error stop 52
END IF



do i = 1, 10
    IF(.not.precision_r16(a1(i), 2.0Q0) ) THEN
      ERROR STOP 54
    END IF
end do
  

do j= 1, 10
   do k = 1, 10 
     IF(.not.precision_r16(a2(k,j),2.0Q0) )THEN
       ERROR STOP 55
     END IF
   end do
end do

do i = 1, 2
   do j = 1, 3
     if ( .not.precision_r16(a3(j,i,1), 2.0Q0) ) then
        error stop 56

     end if
   end do
end do

end 
