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
!* DESCRIPTION                  : Test the interoperability of character and
!*                              : array of character module variable with
!*                              : binding label, use the variable in fortran
!*                              : subroutine, called by main in C.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
character(len = 9), parameter :: kk = "chaRaCter"
character ch1
bind(c, name = "cha1") :: ch1

character*1 ch2
bind(c, name = kk(1:4)) :: ch2

character( len = 1 ) ch3
bind(c, name = "CH3") :: ch3

character, bind(c, name = "  CHa4 ") :: ch4(5)

character :: ch5(3, 2, 1)
bind(c, name=kk) :: ch5

character, dimension(2 ,2), bind(c, name = "  cha6   "(3:9)) :: ch6

end module


subroutine fsub()
use mod

integer :: i, j, k

  IF ( ch1 .NE. 'b') THEN
    ERROR STOP 51
  END IF

  IF ( ch2 .NE. 'c' ) THEN
    ERROR STOP 52
  END IF

  IF ( ch3 .NE. 'd' ) THEN
    ERROR STOP 53
  END IF

  do i = 1, 5
    IF ( ch4(1) .NE. 'e' ) THEN
      ERROR STOP 54
    END IF
  end do


  do j= 1, 2
    do k = 1, 3
     IF ( ch5(k, j, 1) .NE. 'f' ) THEN
       ERROR STOP 55
     END IF
    end do
  end do

  do i = 1, 2
    do j = 1, 2
     if ( ch6(i,j) .NE. 'g') then
        error stop 56
     end if
    end do
  end do

  ch1 = 'a'
  ch2 = 'b'
  ch3 = 'c'
  ch4 = 'd'
  ch5 = 'e'
  ch6 = 'f'


end