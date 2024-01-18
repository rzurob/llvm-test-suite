! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/bcda.sh bcdadcl1
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
!*
!
!* DATE                         : June. 20, 2003
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test the declaration for bind(c)
!*                              : common should appear in the
!*                              : specificatoin part.
!*                              :
!234567890123456789012345678901234567890123456789012345678901234567890
program bcdadcl1
implicit none
integer x
common /blk/ x

x = 1
bind(c) /blk/
end program
