! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : Sept. 18, 2003
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test the binding label must be
!*                              : an initialization character
!*                              : expression.
!234567890123456789012345678901234567890123456789012345678901234567890
program bcdabl07
implicit none
integer x
character*3 c/'foo'/
common /blk/ x
bind(c, name = c) /blk/
end program
