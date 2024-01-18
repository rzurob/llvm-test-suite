!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fselTyp508.f
! %VERIFY: fselTyp508.out:fselTyp508.vf
! %STDIN:
! %STDOUT: fselTyp508.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/13/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : select type (select type construct in function
!                               definition)
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

module m
    type base
        integer id
    end type

    type, extends(base) :: child
        character(20) name
    end type

    contains

    class (base) function rep (b)
        class(base), intent(in) :: b
        allocatable :: rep

        select type (b)
            type is (base)
                allocate (rep)
                rep%id = b%id
            type is (child)
                allocate (child:: rep)

                select type (rep)
                    type is (child)

                    rep%id = - b%id
                    rep%name = b%name
                end select
        end select
    end function
end module

program fselTyp508
use m
    class (base), allocatable :: b1

    associate (x => rep (base(100)))
        select type (y => x)
            type is (base)
                print *, y
            class default
                error stop 1_4
        end select
    end associate

    associate (x => rep(child (10, 'xlftest team')))
        select type (y => x)
            type is (child)
                print *, y
            class default
                error stop 2_4
        end select
    end associate

    allocate (b1, source=child (-1, 'OO team'))

    associate (x => rep (b1))
        select type (y => x)
            type is (child)
                print *, y
            class default
                error stop 3_4
        end select
    end associate
end
