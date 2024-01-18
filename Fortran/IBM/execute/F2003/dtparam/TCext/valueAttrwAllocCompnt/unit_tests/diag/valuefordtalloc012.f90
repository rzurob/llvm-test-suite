! GB DTP extension using:
! ftcx_dtp -ql -qreuse=self /tstdev/F2003/valueAttrwAllocCompnt/unit_tests/diag/valuefordtalloc012.f
! opt variations: -qnol -qreuse=none

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: valuefordtalloc012.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan, 20, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : value attribute for derived types with allocatable components
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : diagnostic testing of %VAL with
!*                               derived types with allocatable
!*                               components
!*
!234567890123456789012345678901234567890123456789012345678901234567890
  module m
    type dt1(n1,k1)    ! (20,4)
      integer, kind         :: k1
      integer, len          :: n1
      integer(k1)           :: i
      real(k1), allocatable :: r
    end type
  end module

  program mainf
    use m

    type(dt1(20,4)) :: temp
    integer :: i

    temp%i=5
    allocate(temp%r)
    temp%r=5.0

    call sub(%VAL(temp))
    i=func(%VAL(temp))

    contains
      subroutine sub(x)
        type(dt1(*,4)) :: x

        x%i=0
        x%r=0.0

      end subroutine

      function func(x)
        type(dt1(*,4)) :: x
        integer :: func

        x%i=0
        x%r=0.0

        func=0
      end function

end program
