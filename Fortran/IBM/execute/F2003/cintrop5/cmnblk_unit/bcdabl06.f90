! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/bcdamix.sh bcdabl06
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         INBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : bcdabl06.f
!
!* PROGRAMMER                   : Helen Li
!* DATE                         : Sept. 18, 2003
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf90
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test in one scope unit, for one
!*                              : bind(c) common, it should only has
!*                              : one binding label, otherwise will
!*                              : issue an error message.
!*                              :
!*                              : Test with -qmixed or without -qmixed
!*                              :
!*                              : Without -qmixed, all other global
!*                              : entities' name is ignoring differences
!*                              : in case.
!*                              :
!*                              : With -qmixed, FE interprets all names
!*                              : in lower case.
!*                              :
!234567890123456789012345678901234567890123456789012345678901234567890
program bcdabl06
implicit none
integer x
common /blk/ x
bind(c, name="foo1") /blk/
bind(c, name="Foo2") /blk/
end program
