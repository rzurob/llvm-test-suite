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
!*                               that does not have the ALLOCATABLE attribute
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
module  mod
  implicit none

  contains
  subroutine module_sub(c)
     type(*), allocatable :: c

  end subroutine module_sub
end module mod

program AssumedType11d
use, intrinsic :: iso_c_binding
implicit none

interface
   subroutine f_sub(a)
      use, intrinsic :: iso_c_binding
      implicit none
      type(*), allocatable :: a
   end subroutine f_sub
   subroutine c_sub(a) BIND(c)
      use, intrinsic :: iso_c_binding
      implicit none
      type(*), allocatable :: a
   end subroutine c_sub
end interface

contains

   subroutine sub(a)
      type(*), allocatable :: a

   end subroutine sub

end program AssumedType11d