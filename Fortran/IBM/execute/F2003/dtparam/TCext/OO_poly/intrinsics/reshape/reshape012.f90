! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/reshape/reshape012.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: reshape012.f
! %VERIFY: reshape012.out:reshape012.vf
! %STDIN:
! %STDOUT: reshape012.out
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
!*    Assigned data entity is non-poly
!*    PAD and ORDER are specified. PAD has different declared type but
!*      same dynamic type as SOURCE
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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: i = 88
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) :: j = 99
    end type
end module

program reshape012
use m
    class(Base(:,4)), pointer :: b1(:) => null()
    type(Base(20,4)) :: c1(3,5)
    class(Child(:,4)), allocatable :: c2(:)

    allocate(b1(10), SOURCE = (/ (Child(20,4)(i,i+100), i=1,10) /))
    allocate(c2(2), SOURCE = (/Child(20,4)(-1,1),Child(20,4)(-2,2)/))

    c1 = reshape(b1, (/3,5/), c2, (/2,1/))

    print *, c1
    select type (b1)
        type is (Child(*,4))
            print *, b1
        class default
            error stop 1_4
    end select
end
