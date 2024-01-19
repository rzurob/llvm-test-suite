!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 20, 2005
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

  type A
    integer, allocatable :: x
  end type

  type(A) :: A1

  allocate(A1%x)

  A1%x=5

  call sub(A1)
  if(A1%x .ne. 5) error stop 1

   contains
     subroutine sub(A2)
       type(A), value :: A2
       if(A1%x .ne. 5) error stop 2
       A2%x=6
     end subroutine

end program