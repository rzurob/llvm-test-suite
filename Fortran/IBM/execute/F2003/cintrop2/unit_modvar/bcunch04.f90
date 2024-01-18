! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : May. 24, 2003
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    : -qfree=f90
!*
!* DESCRIPTION                  : Test bind(c) attribute work as
!*                              : character*1.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
character*1, bind ( c , name = " foo " ) :: ch
end module
program bcunch04
end program
