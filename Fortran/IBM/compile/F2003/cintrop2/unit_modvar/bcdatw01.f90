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
!* DESCRIPTION                  : Test the bind(c) attribute should
!*                              : not specified twice for same variable.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
integer, bind(c) :: x
bind(c) x
end module
program bcdatw01
end program
