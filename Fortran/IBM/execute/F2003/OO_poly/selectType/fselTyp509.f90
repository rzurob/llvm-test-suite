!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fselTyp509.f
! %VERIFY: fselTyp509.out:fselTyp509.vf
! %STDIN:
! %STDOUT: fselTyp509.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : select type (selector's dynamic type is of
!                               character)
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

program fselTyp509
    class (*), allocatable :: x

    allocate (x, source='xlftest 101')

    select type (x)
        type is (character(*))
            print *, x
        class default
            print *, 'bad'
    end select

    select type (y => x)
        type is (character(*))
            print *, y(1:7)
    end select
end

