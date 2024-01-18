! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=base /tstdev/OO_poly/intrinsics/spread/structureComponent001.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=self -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: structureComponent001.f
! %VERIFY: structureComponent001.out:structureComponent001.vf
! %STDIN:
! %STDOUT: structureComponent001.out
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
!*    SOURCE is a structure component, which is non-poly
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
    type, abstract :: AbstractParent(k1)    ! (4)
        integer, kind :: k1
    end type

    type, extends(AbstractParent) :: Base    ! (4)
        integer(k1) i
    end type

    type Base1(k2,k3)    ! (4,4)
        integer, kind :: k2,k3
        integer(k2)      m
        integer(k3)      n
    end type

    type, extends(Base) :: Child    ! (4)
        type(Base(k1)) :: b1(20)
        type(Base1(k1,k1)) :: b2(5,5)
    end type
end module

program structureComponent001
use m
    type(Child(4)) :: c1

    c1%b1 = (/ (Base(4)(i), i=1,20) /)
    c1%b2 = reshape((/(Base1(4,4)(i,-i),i=1,20)/), (/5,5/), &
     (/Base1(4,4)(88,99)/), (/2,1/))

    associate(name1=>spread(c1%b1, 2, 2))
        if(.NOT. same_type_as(name1, Base(4)(1))) error stop 1_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate

    associate(name1=>spread(c1%b2, 3, 2))
        if(.NOT. same_type_as(name1, Base1(4,4)(1,1))) error stop 2_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate
end
