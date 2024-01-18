!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc001a7.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/27/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (use of length of named constant that
!                               is declared with (*) for a character type in
!                               ALLOCATE statement)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program falloc001a7
    character(*), parameter :: ch_const = 'xlftest'

    character(len(ch_const)), pointer :: ch1
    character (7), pointer :: ch2(:)

    allocate (character(kind=kind(ch_const), len=len(ch_const)) :: ch1)
    allocate (ch2(2:3), source=ch_const)

    ch1 = ch_const(4:7)//ch_const(1:3)

    if (ch1 /= 'testxlf') error stop 1_4

    if (any (ch2 /= 'xlftest')) error stop 2_4
end
