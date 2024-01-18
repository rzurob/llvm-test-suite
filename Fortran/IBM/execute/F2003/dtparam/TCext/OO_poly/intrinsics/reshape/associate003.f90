! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/intrinsics/reshape/associate003.f
! opt variations: -qnok -ql -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: associate003.f
! %VERIFY: associate003.out:associate003.vf
! %STDIN:
! %STDOUT: associate003.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 10/25/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                : SOURCE is an associate name. Selector
!*    is array section.
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

program associate003
use m
    class(AbstractParent(4)), pointer :: ap1(:,:) => null()
    class(AbstractParent(4)), allocatable :: ap2(:,:,:)
    class(AbstractParent(4)), allocatable :: p(:)

    allocate(p(3), SOURCE=(/Base(4)(-1),Base(4)(-2),Base(4)(-3)/))

    allocate(ap1(5,6), SOURCE=reshape((/(Base(4)(i),i=1,40)/), (/5,6/)))

    associate(name1=>ap1(3:,1:6:2))
        allocate(ap2(2,3,2), SOURCE= &
         reshape(name1, (/2,3,2/), p, (/1,2,3/)))
    end associate

    select type (ap2)
        type is (Base(4))
            print *, ap2
        class default
            error stop 1_4
    end select
end
