! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : June. 20, 2003
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test if a common block has the BIND(C)
!*                              : attribute, then it must be declared to
!*                              : have the BIND(C) attribute with the same
!*                              : binding label in all scoping unites
!*                              : in which it is declared.
!234567890123456789012345678901234567890123456789012345678901234567890
program bcdasc06
implicit none
integer x
bind(c, name="first") /blk/
common /blk/ x
call sub
contains
subroutine sub
implicit none
integer y
bind(c, name="second") /blk/
common /blk/ y
end subroutine
end program
