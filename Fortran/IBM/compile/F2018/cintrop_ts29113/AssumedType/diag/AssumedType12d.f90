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
!*                               that does not have the POINTER attribute
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
module  mod
  implicit none

  contains
  subroutine module_sub(c)
     type(*), pointer :: c

  end subroutine module_sub
end module mod

program AssumedType12d
use, intrinsic :: iso_c_binding
implicit none

interface
   subroutine f_sub(a)
      use, intrinsic :: iso_c_binding
      implicit none
      type(*), pointer :: a
   end subroutine f_sub
   subroutine c_sub(a) BIND(c)
      use, intrinsic :: iso_c_binding
      implicit none
      type(*), pointer :: a
   end subroutine c_sub
end interface

contains

   subroutine sub(a)
      type(*), pointer :: a

   end subroutine sub

end program AssumedType12d
