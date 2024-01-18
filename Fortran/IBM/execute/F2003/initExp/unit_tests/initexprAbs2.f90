!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : ABS on initializaiton expression
!*
!* DESCRIPTION                : COMPLEX type
!* ===================================================================
       program main
       complex(4), parameter :: y4=(-2.0E0, -5e0)
       complex(8), parameter :: y8=(1.3_8, -2.0D0)
       complex(16), parameter :: y16=(-2.1Q0, 1.98_16)
       real(4) :: x4=abs(y4)
       real(8) :: x8=abs(y8)
       real(16) :: x16=abs(y16)

       if (abs(x4 - abs(y4)) > 1.0E-5) then
         write(*,'(2z10.8)') x4, abs(y4)
         stop 1
       endif
       if (x8 /= abs(y8)) stop 2
       if (x16 /= abs(y16)) stop 3

       end program
