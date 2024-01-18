! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/fxmdvn01.sh fxmdvp09 cxmdvp09
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
!* TEST CASE TITLE              : fxmdvp09.f
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
!* DESCRIPTION                  : Test the interoperability of logical and
!*                              : array of logical module variable with binding
!*                              : label, use the variable in fortran subroutine,
!*                              : which is called by main in C.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
character(7), parameter :: kk = "lOgical"
logical(1) :: lo = .true.
bind(c, name = "LLLOOOOO  "(3:4)) :: lo

logical(1) :: lo1(10)
bind(c, name = "LO1") lo1

logical(1), bind(c, name = kk) :: lo3(3, 2, 2)

logical(1), dimension(10 ,10), bind(c, name = "    LO2  ") :: lo2

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
     if ( lo2(i,j) .NEQV. .false.) then
        error stop 54
     end if
    end do
  end do
  
  lo = .true.
  lo1 = .true.
  lo2 = .true.
  lo3 = .true.


end 
