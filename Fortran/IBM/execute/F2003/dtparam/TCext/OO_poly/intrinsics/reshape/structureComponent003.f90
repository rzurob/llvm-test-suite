! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/intrinsics/reshape/structureComponent003.f
! opt variations: -qnok -ql -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
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
!*  DATE                       : 12/06/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                : SOURCE is a structure component, which
!*    is unlimited poly array. The object containing the component is a
!*    scalar.
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
        class(*), allocatable :: b2(:,:)
    end type
end module

program structureComponent003
use m
    class(*), allocatable :: b0(:,:,:)
    type(Base(4)) :: b1(20)
    type(Child(4)) :: c1

    b1 = (/ (Base(4)(i), i=1,20) /)

    allocate(c1%b2(5,5), SOURCE=reshape(b1, (/5,5/), &
     (/Base(4)(-1),Base(4)(-2)/), (/2,1/)))

    allocate(b0(3,2,4), SOURCE=reshape(c1%b2, (/3,2,4/), &
     (/Base(4)(-3)/), (/3,2,1/)))

    print *, b1

    select type (b0)
        type is (Base(4))
            print *, b0
        class default
            error stop 1_4
    end select
end
