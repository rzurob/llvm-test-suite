!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc002.f
! %VERIFY: falloc002.out:falloc002.vf
! %STDIN:
! %STDOUT: falloc002.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (assumed-character-length in type-spec
!                               in ALLOCATE statement)
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

program falloc002

    character(5), pointer :: c1 => null()

    call test1 (c1)

    deallocate (c1)

    contains

    subroutine test1 (c)
        character(*), pointer, intent(inout) :: c

        allocate (character(*) :: c)

        c1 = 'xlftest'

        print *, c1
    end subroutine
end
