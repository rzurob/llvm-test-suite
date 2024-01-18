! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: associate004.f
! %VERIFY: associate004.out:associate004.vf
! %STDIN:
! %STDOUT: associate004.out
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

program associate004
use m
    class(*), allocatable :: ap1(:,:)
    type(Child) :: c1(5)

    allocate(ap1(4,5), SOURCE=reshape((/(Base(i),i=1,20)/), (/4,5/)))

    associate(name1=>ap1(3,:), name2=>ap1(2:4:2,2:5:3))
        select type (name3=>transfer(name2, name1))
            type is (Base)
                print *, name3
            class default
                error stop 1_4
        end select
    end associate
end
