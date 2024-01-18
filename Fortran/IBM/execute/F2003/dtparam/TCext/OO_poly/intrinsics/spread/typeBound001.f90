! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=base /tstdev/OO_poly/intrinsics/spread/typeBound001.f
! opt variations: -qnok -qnol -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: typeBound001.f
! %VERIFY: typeBound001.out:typeBound001.vf
! %STDIN:
! %STDOUT: typeBound001.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/18/2005
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Cross testing type bound.
!*    Non-poly
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
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains
        procedure :: spreadMe
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4,20)
        integer(k1) j
    end type

    contains

    subroutine spreadMe(this, i)
        class(AbstractParent(4,*)), intent(in) :: this
        integer, intent(in) :: i
        associate(name1=>spread(this, 1, i))
            select type(name1)
                type is (Base(4,*))
                    print *, "Base", name1
                    print *, size(name1)
                    print *, shape(name1)
                type is (Child(4,*))
                    print *, "Child", name1
                    print *, size(name1)
                    print *, shape(name1)
                class default
                    error stop 1_4
            end select
        end associate
    end subroutine
end module

program typeBound001
use m
    type(Base(4,20)) :: b1
    type(Child(4,20)) :: c1

    b1%i = 1
    c1%i = 2
    c1%j = 3

    call b1%spreadMe(10)
    call c1%spreadMe(8)
end
