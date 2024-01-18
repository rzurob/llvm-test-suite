! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=base /tstdev/OO_poly/intrinsics/reshape/associate002.f
! opt variations: -qnok -qnol -qreuse=none

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
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 12/05/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                : SOURCE is an associate name. Unlimited
!*    poly.
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
        integer(k1) :: i = 8
    end type

    type, extends(Base) :: Child    ! (4,20)
        integer(k1) :: j = 9
    end type
end module

program associate002
use m
    class(*), pointer :: ap1(:,:) => null()
    class(*), allocatable :: ap2(:,:)
    class(*), allocatable :: p(:)

    allocate(Child(4,20)::p(2))

    allocate(ap1(4,4), SOURCE= &
     reshape((/(Child(4,20)(i,i+1),i=1,20)/),(/4,4/)))

    associate(name1=>ap1)
        allocate(ap2(3,5), SOURCE=reshape(name1, (/3,5/), p, (/2,1/)))
    end associate

    select type (ap1)
        type is (Child(4,*))
            print *, ap1
        class default
            error stop 1_4
    end select

    select type (ap2)
        type is (Child(4,*))
            print *, ap2
        class default
            error stop 2_4
    end select
end
