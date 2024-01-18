! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/valueAttrwAllocCompnt/unit_tests/func/valuefordtalloc001.f
! opt variations: -qnol

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: valuefordtalloc001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 20, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : value attribute for derived types with allocatable components
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : functional testing of value attribute
!*                               for derived types with allocatable
!*                               components
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  type A(n1,k1)    ! (20,4)
    integer, kind            :: k1
    integer, len             :: n1
    integer(k1), allocatable :: x
  end type

  type(A(20,4)) :: A1

  allocate(A1%x)

  A1%x=5

  call sub(A1)
  if(A1%x .ne. 5) error stop 1

   contains
     subroutine sub(A2)
       type(A(20,4)), value :: A2
       if(A1%x .ne. 5) error stop 2
       A2%x=6
     end subroutine

end program
