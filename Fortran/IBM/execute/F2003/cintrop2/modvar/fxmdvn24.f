! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/fxmdvn01.sh fxmdvn24 cxmdvn08 
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
!* XL Fortran Test Case                         INBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : fxmdvn24.f
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
!* DESCRIPTION                  : Test the interoperability of character and
!*                              : array of character module variable with 
!*                              : binding label, use iso_c_binding
!234567890123456789012345678901234567890123456789012345678901234567890
module mod

use iso_c_binding

character(len = 9), parameter :: kk = "chaRaCter"
character ch1
bind(c, name = "cha1") :: ch1

character(c_char) :: ch2
bind(c, name = kk(1:4)) :: ch2

character( c_char ) ch3
bind(c, name = "CH3") :: ch3

character(c_char), bind(c, name = "  CHa4 ") :: ch4(5)

character(c_char) :: ch5(3, 2, 1)
bind(c, name=kk) :: ch5

character(c_char), dimension(2 ,2), bind(c, name = "  cha6   "(3:9)) :: ch6

end module

use mod

integer :: i, j, k

ch1 = 'a'
ch2 = 'b'
ch3 = 'c'
ch4 = 'd'
ch5 = 'e'
ch6 = 'f'

call csub()

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

end 
