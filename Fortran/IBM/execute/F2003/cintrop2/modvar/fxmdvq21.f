! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/fxmdvn01.sh fxmdvq21 cxmdvq03
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
!* DESCRIPTION                  : Test the interoperability of logical and
!*                              : array of logical module variable. Use the
!*                              : variables in fmain and csub which calls the
!*                              : fsub. Use module iso_c_binding
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
use iso_c_binding
logical(c_bool) :: lo = .true.
bind(c) lo

logical(c_bool) :: lo1(10)
bind(c) lo1

logical(c_bool), bind(c) :: lo3(3, 2, 2)

logical(c_bool), dimension(10 ,10), bind(c) :: lo2

end module

use mod

integer :: i, j, k

if ( lo .NEQV. .true.) then
   error stop 50
end if

lo1 = .true.
lo2 = .true.
lo3 = .true.

call csub()

  IF ( lo .NEQV. .true. ) THEN
    ERROR STOP 51
  END IF



  do i = 1, 10
    IF ( lo1(i) .NEQV. .true. ) THEN
      ERROR STOP 52
    END IF
  end do


  do j= 1, 2
    do k = 1, 3
      do i = 1, 2
        IF ( lo3(k, j, i) .NEQV. .true. ) THEN
          ERROR STOP 53
        END IF
      end do
    end do
  end do

  do i = 1, 2
    do j = 1, 2
     if ( lo2(i,j) .NEQV. .true.) then
        error stop 54
     end if
    end do
  end do

end

subroutine fsub()
use mod

lo  = .false.
lo1 = .false.
lo2 = .false.
lo3 = .false.

end

