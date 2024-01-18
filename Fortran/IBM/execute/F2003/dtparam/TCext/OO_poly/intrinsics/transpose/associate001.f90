! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/intrinsics/transpose/associate001.f
! opt variations: -qnok -ql -qreuse=none

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
!*  DATE                       : 12/31/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    MATRIX is an associate name.
!*    Selector is a polymorphic whole array.
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
    type, abstract :: AbstractParent(k1)    ! (4)
        integer, kind :: k1
    end type

    type, extends(AbstractParent) :: Base    ! (4)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type
end module

program associate001
use m
    class(AbstractParent(4)), pointer :: ap1(:,:) => null()

    allocate(ap1(4,6), SOURCE=reshape((/(Base(4)(i),i=1,24)/),(/4,6/)))

    associate(name1=>ap1)
        select type(name2=>transpose(name1))
            type is (Base(4))
                print *, name2
            class default
                error stop 1_4
        end select
    end associate
end
