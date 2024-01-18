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
!* DESCRIPTION                  : Test the interoperability of real16 module
!*                              : variable with corresponding c variable
!*                              : compiled with -qlongdouble. The variable is
!*                              : in scope of Fortran sub, which called by main
!*                              : in C.
!234567890123456789012345678901234567890123456789012345678901234567890

module mod

real(16) :: a = 0.0Q0
bind(c) :: a

real*16, bind(c) :: aa = 0.0Q0

real(16), dimension(10), bind(c) :: a1
real(16), dimension(10, 10), bind(c) :: a2
real(16), bind(c) :: a3(3, 2, 1)

complex(16) :: b = (0.0Q0, 1.0Q0)
bind(c) :: b



end module

subroutine fsub()
use mod

logical precision_r16


IF ( .not.precision_r16(a, 2.0Q0) ) THEN
    ERROR STOP 51
END IF

IF ( .not.precision_r16(aa, 2.0Q0)  ) THEN
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


a  = 0.0Q0
aa = 0.0Q0
a1 = 0.0Q0
a2 = 0.0Q0
a3 = 0.0Q0


end
