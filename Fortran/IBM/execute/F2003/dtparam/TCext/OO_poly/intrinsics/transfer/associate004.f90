! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=none /tstdev/OO_poly/intrinsics/transfer/associate004.f
! opt variations: -qnok -qnol -qreuse=base

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
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(AbstractParent) :: Base(n2,k2)    ! (4,20,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      i
    end type

    type, extends(Base) :: Child(n3,k3)    ! (4,20,20,4,20,4)
        integer, kind :: k3
        integer, len  :: n3
        integer(k3)      j
    end type
end module

program associate004
use m
    class(*), allocatable :: ap1(:,:)
    type(Child(4,20,20,4,20,4)) :: c1(5)

    allocate(ap1(4,5), SOURCE=reshape((/(Base(4,20,20,4)(i),i=1,20)/), (/4,5/)))

    associate(name1=>ap1(3,:), name2=>ap1(2:4:2,2:5:3))
        select type (name3=>transfer(name2, name1))
            type is (Base(4,*,*,4))
                print *, name3
            class default
                error stop 1_4
        end select
    end associate
end
