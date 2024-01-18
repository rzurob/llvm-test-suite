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
!* DESCRIPTION                  : Test the binding label must be
!*                              : character initialization expression.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
character*3 nm/'foo'/
integer, bind(c, name=nm) :: x
end module
