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
!* DESCRIPTION                  : Test the interoperability of logical and
!*                              : array of logical module variable with binding
!*                              : label, use iso_c_binding. In the scope of
!*                              : fortran subroutine and C main.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod

use iso_c_binding

character(7), parameter :: kk = "lOgical"

logical(c_bool) :: lo = .true.
bind(c, name = "LLLOOOOO  "(3:4)) :: lo

logical(c_bool) :: lo1(10)
bind(c, name = "lo") lo1

logical(c_bool), bind(c, name = "kk") :: lo3(3, 2, 2)

logical(c_bool), dimension(10 ,10), bind(c, name = "KK") :: lo2

logical(c_bool), dimension(10, 10), bind(c, name = "lo2") :: KK

end module


subroutine fsub()
use mod

integer :: i, j, k


  IF ( lo .NEQV. .false. ) THEN
    ERROR STOP 51
  END IF



  do i = 1, 10
    IF ( lo1(i) .NEQV. .false. ) THEN
      ERROR STOP 52
    END IF
  end do


  do j= 1, 2
    do k = 1, 3
      do i = 1, 2
        IF ( lo3(k, j, i) .NEQV. .false. ) THEN
          ERROR STOP 53
        END IF
      end do
    end do
  end do

  do i = 1, 2
    do j = 1, 2
     if ( lo2(i,j) .NEQV. .false. ) then
        error stop 54
     end if
    end do
  end do

  do i = 1, 2
    do j = 1, 2
     if ( KK(i,j) .NEQV. .true. ) then
        error stop 54
     end if
    end do
  end do


lo  = .true.
lo1 = .true.
lo2 = .true.
lo3 = .true.
KK  = .false.


end
