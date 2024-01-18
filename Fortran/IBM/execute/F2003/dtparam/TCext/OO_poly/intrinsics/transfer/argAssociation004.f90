! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qreuse=base /tstdev/OO_poly/intrinsics/transfer/argAssociation004.f
! opt variations: -ql -qdefaultpv -qreuse=self -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: argAssociation004.f
! %VERIFY: argAssociation004.out:argAssociation004.vf
! %STDIN:
! %STDOUT: argAssociation004.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/29/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE or MOLD of TRANSFER is a dummy argument. Dummy argument
!*  is non-pointer, non-allocatable, poly, and is array.
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type

    type Base1(k2,k3,k4)    ! (4,4,4)
        integer, kind  :: k2,k3,k4
        integer(k2)       j
        type(Base(k2)) :: k
        integer(k3)       m
        integer(k4)       n
    end type

    contains

    subroutine sub1(arg1, arg2, arg3, arg4)
        class(Base(4)) :: arg1(10)
        class(Base(4)) :: arg2(:,:)
        class(Base(4)) :: arg3(:)
        class(Base(4)) :: arg4(2,2)

        select type (name1=>transfer(arg1, arg3))
            type is (Child(4))
                print *, name1
            class default
                error stop 1_4
        end select

        associate(name1=>transfer(arg2, Base(4)(1), 8))
            print *, name1
            if(.NOT. same_type_as(name1, Base(4)(1))) error stop 2_4
        end associate

        associate(name1=>transfer(arg4, (/Base1(4,4,4)(1,Base(4)(1),1,1)/)))
            print *, name1
            if(.NOT. same_type_as(name1, Base1(4,4,4)(1,Base(4)(1),1,1))) &
             error stop 3_4
        end associate
    end subroutine
end module

program argAssociation004
use m
    type(Base(4)) :: b1(10)
    type(Child(4)) :: c1(2,3)
    class(Base(4)), pointer :: b2(:)
    class(Child(4)), allocatable :: c2(:,:)

    b1 = (/ (Base(4)(i),i=1,10) /)
    c1 = reshape((/(Child(4)(i, i+1),i=5,15,2)/),(/2,3/))
    allocate(b2(6), SOURCE=(/(Child(4)(i,i+1),i=2,7)/))
    allocate(c2(2,2), SOURCE=reshape((/(Child(4)(j=i-1,i=i), &
     i=12,15)/), (/2,2/)))

    call sub1(b1, c1, b2, c2)
end
