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
!*  DESCRIPTION                : An assumed-type entity shall be a dummy variable
!*                               that does not have the CODIMENSION attribute
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
module  mod
  implicit none

  contains
  subroutine module_sub(coa)
      type(*), codimension[*] :: coa

  end subroutine module_sub
end module mod

program AssumedType20d
implicit none

contains

   subroutine sub(coa)
      type(*), codimension[*] :: coa

   end subroutine sub

end program AssumedType20d
