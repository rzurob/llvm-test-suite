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
!* DESCRIPTION                  : Test the bind(c) attribute must not
!*                              : be specified for the equivalence
!*                              : members.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
integer, bind(c) :: x
integer y(2,3)
equivalence (x, y(1,1))
end module