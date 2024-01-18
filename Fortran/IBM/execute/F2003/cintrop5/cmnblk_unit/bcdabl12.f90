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
!* DESCRIPTION                  : Test the binding label can not be
!*                              : specified twice. A Warning message
!*                              : will be issued.
!*                              : Test with -qmixed or without -qmixed
!*                              : Without -qmixed, all other global
!*                              : entities' name is ignoring differences
!*                              : in case.
!*                              : With -qmixed, FE interprets all names
!*                              : in lower case.
!234567890123456789012345678901234567890123456789012345678901234567890
program bcdabl12
implicit none
integer x
common /Blk/ x
bind(c, name="Foo") /Blk/
bind(c, name="Foo") /Blk/
end program
