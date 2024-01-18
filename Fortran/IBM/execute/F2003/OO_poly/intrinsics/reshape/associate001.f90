! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: associate001.f
! %VERIFY: associate001.out:associate001.vf
! %STDIN:
! %STDOUT: associate001.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/05/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                : SOURCE is an associate name.
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

program associate001
use m
    class(AbstractParent), pointer :: ap1(:) => null()
    class(AbstractParent), allocatable :: ap2(:,:)

    allocate(ap1(20), SOURCE=(/ (Base(i),i=1,20) /))

    associate(name1=>ap1)
        allocate(ap2(3,5), SOURCE=reshape(name1, (/3,5/), &
         (/Base(-1)/), (/2,1/)))
    end associate

    select type (ap2)
        type is (Base)
            print *, ap2
        class default
            error stop 1_4
    end select
end
