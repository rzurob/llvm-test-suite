! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/intrinsics/transfer/associate002.f
! opt variations: -qnok -ql -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: associate002.f
! %VERIFY: associate002.out:associate002.vf
! %STDIN:
! %STDOUT: associate002.out
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
!*    SOURCE is an associate name.
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

program associate002
use m
    class(AbstractParent(4)), pointer :: ap1(:,:) => null()
    type(Child(4)) :: c1(5)

    allocate(ap1(4,5), SOURCE=reshape((/(Base(4)(i),i=1,20)/), (/4,5/)))

    associate(name1=>ap1(:,3))
        associate(name2=>transfer(name1, c1))
            if(.NOT. same_type_as(name2, c1)) error stop 1_4
            print *, size(name2)
            print *, name2
        end associate
    end associate

    associate(name1=>ap1(2:4,2:3))
        associate(name2=>transfer(name1, c1, 2))
            if(.NOT. same_type_as(name2, c1)) error stop 2_4
            print *, size(name2)
            print *, name2
        end associate
    end associate
end
