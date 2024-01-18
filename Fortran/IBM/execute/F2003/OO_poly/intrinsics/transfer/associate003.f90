! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: associate003.f
! %VERIFY: associate003.out:associate003.vf
! %STDIN:
! %STDOUT: associate003.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/21/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE and/or MOLD are associate names.
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  ===================================================================
!*  REVISION HISTORY
!*                    MM/DD/YY :
!*                        Init :
!*                    Comments :
!*  ===================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module m
    type, abstract :: AbstractParent
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

program associate003
use m
    class(AbstractParent), pointer :: ap1(:) => null()

    allocate(ap1(20), SOURCE=(/ (Base(i),i=1,20) /))

    associate(name1=>ap1)
        select type (name2=>transfer(Child(8,9), name1))
            type is (Base)
                print *, name2
            class default
                error stop 1_4
        end select
    end associate

    associate(name1=>ap1(3:7), name2=>ap1(10:))
        select type (name3=>transfer(name1, name2))
            type is (Base)
                print *, name3
            class default
                error stop 2_4
        end select
    end associate

    associate(name1=>ap1(15))
        select type (name2=>transfer(Child(8,9), name1))
            type is (Base)
                print *, name2
            class default
                error stop 3_4
        end select
    end associate
end
