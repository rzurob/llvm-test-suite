! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=none /tstdev/OO_poly/intrinsics/spread/structureComponent003.f
! opt variations: -qnok -qnol -qreuse=self -qreuse=base

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: structureComponent003.f
! %VERIFY: structureComponent003.out:structureComponent003.vf
! %STDIN:
! %STDOUT: structureComponent003.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 01/18/2005
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    SOURCE is a structure component, which is unlimited poly
!*  array. The object containing the component is a scalar.
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

    type Base1(n3,k3,k4)    ! (20,4,4)
        integer, kind :: k3,k4
        integer, len  :: n3
        integer(k3)      m
        integer(k4)      n
    end type

    type, extends(Base) :: Child(k5,n4)    ! (4,20,20,4,4,20)
        integer, kind :: k5
        integer, len  :: n4
        class(*), pointer :: b1
        class(*), pointer :: b2(:)
        class(*), allocatable :: b3(:,:)
    end type
end module

program structureComponent003
use m
    type(Child(4,20,20,4,4,20)) :: c1

    allocate(c1%b1, SOURCE=Base(4,20,20,4)(8))

    allocate(c1%b2(10), SOURCE=(/(Base(4,20,20,4)(i),i=1,10)/))

    allocate(c1%b3(5,5), SOURCE=reshape((/(Base1(20,4,4)(i,-i),i=1,20)/), &
     (/5,5/), (/Base1(20,4,4)(-1,-2),Base1(20,4,4)(-3,-4)/), (/2,1/)))

    select type(name1=>spread(c1%b1, 1, 10))
        type is (Base(4,*,*,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    select type(name1=>spread(c1%b2, 2, 2))
        type is (Base(4,*,*,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select

    select type(name1=>spread(c1%b3, 3, 2))
        type is (Base1(*,4,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 3_4
    end select
end
