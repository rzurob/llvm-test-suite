!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 13, 2012
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop: Assumed Type objects
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : An assumed-type entity shall be a dummy variable
!*                               that does not have the VALUE attribute
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
module  mod
  implicit none

  contains
  subroutine module_sub(c)
     type(*), value :: c

  end subroutine module_sub
end module mod

program AssumedType18d
implicit none

interface
   subroutine f_sub(a)
      implicit none
      type(*), value :: a
   end subroutine f_sub
   subroutine c_sub(a) BIND(c)
      implicit none
      type(*), value :: a
   end subroutine c_sub
end interface

contains

   subroutine sub(a)
      type(*), value :: a

   end subroutine sub

end program AssumedType18d
