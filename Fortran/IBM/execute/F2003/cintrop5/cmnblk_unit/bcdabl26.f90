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
!* DESCRIPTION                  : Test in one scope unit, for one
!*                              : bind(c) common, it should only has
!*                              : one binding label, otherwise will
!*                              : issue an error message.
!*                              : Test with -qmixed or without -qmixed
!*                              : Without -qmixed, all other global
!*                              : entities' name is ignoring differences
!*                              : in case.
!*                              : With -qmixed, FE interprets all names
!*                              : in lower case.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
implicit none
integer x
common /blk/ x
bind(c, name="foo1") /blk/
bind(c, name="Foo2") /blk/
end module
program bcdabl26
end program
