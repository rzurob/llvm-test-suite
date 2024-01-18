! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/spread/structureComponent002.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: structureComponent002.f
! %VERIFY: structureComponent002.out:structureComponent002.vf
! %STDIN:
! %STDOUT: structureComponent002.out
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
!*    SOURCE is a structure component, which is poly array.
!*  The object containing the component is a scalar.
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

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type Base1(n2,k2,k3)    ! (20,4,4)
        integer, kind :: k2,k3
        integer, len  :: n2
        integer(k2)      m
        integer(k3)      n
    end type

    type, extends(Base) :: Child    ! (4,20)
        class(Base(k1,:)), pointer :: b1(:)
        class(Base1(:,k1,k1)), allocatable :: b2(:,:)
    end type
end module

program structureComponent002
use m
    type(Child(4,20)) :: c1

    allocate(c1%b1(10), SOURCE=(/(Base(4,20)(i),i=1,10)/))

    allocate(c1%b2(5,5), SOURCE=reshape((/(Base1(20,4,4)(i,-i),i=1,20)/), &
     (/5,5/), (/Base1(20,4,4)(-1,-2),Base1(20,4,4)(-3,-4)/), (/2,1/)))

    select type(name1=>spread(c1%b1, 2, 2))
        type is (Base(4,*))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    select type(name1=>spread(c1%b2, 3, 2))
        type is (Base1(*,4,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
