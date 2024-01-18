! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/reshape/reshape018.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: reshape018.f
! %VERIFY: reshape018.out:reshape018.vf
! %STDIN:
! %STDOUT: reshape018.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/20/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is poly
!*    Assigned data entity is unlimited poly
!*    PAD and ORDER are specified. PAD has same declared type as SOURCE
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
        integer(k1) :: i = 88
    end type

    type, extends(Base) :: Child    ! (4,20)
        integer(k1) :: j = 99
    end type
end module

program reshape018
use m
    class(AbstractParent(4,:)), pointer :: b1(:) => null()
    class(*), allocatable :: c1(:,:)
    class(AbstractParent(4,:)), allocatable :: b2(:)

    allocate(b1(10), SOURCE = (/ (Base(4,20)(i), i=1,10) /))
    allocate(b2(2), SOURCE = (/Base(4,20)(-1),Base(4,20)(-2)/))
    allocate(c1(3,5), SOURCE = reshape(b1, (/3,5/), b2, (/2,1/)))

    select type (b1)
        type is (Base(4,*))
            print *, b1
        class default
            error stop 1_4
    end select

    select type (c1)
        type is (Base(4,*))
            print *, c1
        class default
            error stop 2_4
    end select
end
