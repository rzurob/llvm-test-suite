! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/fxmdvn01.sh fxmdvq02 cxmdvq02
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
!* DESCRIPTION                  : Test the interoperability of character and
!*                              : array of character module variable. use the
!*                              : variables in fmain and csub which calls fsub.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
character ch1
bind(c) ch1

character*1 ch2
bind(c) ch2

character( len = 1 ) ch3
bind(c) ch3

character, bind(c) :: ch4(5)

character :: ch5(3, 2, 1)
bind(c) ch5

character, dimension(2 ,2), bind(c) :: ch6

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

  IF ( ch1 .NE. 'c') THEN
    ERROR STOP 51
  END IF

  IF ( ch2 .NE. 'd' ) THEN
    ERROR STOP 52
  END IF

  IF ( ch3 .NE. 'e' ) THEN
    ERROR STOP 53
  END IF

  do i = 1, 5
    IF ( ch4(1) .NE. 'f' ) THEN
      ERROR STOP 54
    END IF
  end do


  do j= 1, 2
    do k = 1, 3
     IF ( ch5(k, j, 1) .NE. 'g' ) THEN
       ERROR STOP 55
     END IF
    end do
  end do

  do i = 1, 2
    do j = 1, 2
     if ( ch6(i,j) .NE. 'h') then
        error stop 56
     end if
    end do
  end do

end

subroutine fsub()
use mod

ch1 = 'b'
ch2 = 'c'
ch3 = 'd'
ch4 = 'e'
ch5 = 'f'
ch6 = 'g'

end


