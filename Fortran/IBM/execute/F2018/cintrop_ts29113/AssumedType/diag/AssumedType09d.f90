!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 13, 2012
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop: Assumed Type objects
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : TYPE(*) cannot appear in type-spec of
!*                               an array constructor
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
module  mod
  implicit none

  contains
  subroutine module_sub(a, b)
     type(*) :: a(:), b(:)

      a = [ TYPE(*) :: b(1), b(5), b(10) ]

  end subroutine module_sub
end module mod
program AssumedType09d
implicit none


contains

   subroutine sub(a, b)
      type(*) :: a(:), b(:)

      a = [ TYPE(*) :: b(1), b(5), b(10) ]

   end subroutine sub

end program AssumedType09d
