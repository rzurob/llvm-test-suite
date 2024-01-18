! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/fxmdvn01.sh fxmdvn03 cxmdvn03
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
!* TEST CASE TITLE              : fxmdvn03.f
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
!*                              : array of logical module variable.
!*                              : Similar to fxmdvn01.f but test logical type.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod

logical(1) :: lo = .true.
bind(c) lo

logical(1) :: lo1(10)
bind(c) lo1

logical(1), bind(c) :: lo3(3, 2, 2)

logical(1), dimension(10 ,10), bind(c) :: lo2
end module


use mod

integer :: i, j, k

!check the initialization in the module.
if ( lo .NEQV. .true.) then
   error stop 50
end if

lo1 = .true.
lo2 = .true.
lo3 = .true.

call csub()

!check the values after C subruntine call.
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

end 
