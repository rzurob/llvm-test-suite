! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : May. 24, 2003
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test bindling label work with
!*                              : -qmixed.
!234567890123456789012345678901234567890123456789012345678901234567890
module Mod
integer, bind(c, name="Mod") :: x
end module
