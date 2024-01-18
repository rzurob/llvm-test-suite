! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/OO_poly/intrinsics/reshape/reshape013.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=base

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: reshape013.f
! %VERIFY: reshape013.out:reshape013.vf
! %STDIN:
! %STDOUT: reshape013.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 11/20/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    SOURCE is poly
!*    Assigned data entity is poly
!*    PAD and ORDER are not specified
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
        integer(k2)   :: i = 88
    end type

    type, extends(Base) :: Child(n3,k3)    ! (4,20,20,4,20,4)
        integer, kind :: k3
        integer, len  :: n3
        integer(k3)   :: j = 99
    end type
end module

program reshape013
use m
    class(AbstractParent(4,:)), pointer :: b1(:) => null()
    class(AbstractParent(4,:)), allocatable :: c1(:,:)
    allocate(b1(20), SOURCE = (/ (Base(4,20,20,4)(i), i=1,20) /))

    allocate(c1(3,5), SOURCE = reshape(b1, (/3,5/)))

    select type (b1)
        type is (Base(4,*,*,4))
            print *, b1
        class default
            error stop 1_4
    end select

    select type (c1)
        type is (Base(4,*,*,4))
            print *, c1
        class default
            error stop 2_4
    end select
end
