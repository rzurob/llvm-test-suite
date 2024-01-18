! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/intrinsics/reshape/reshape017.f
! opt variations: -qnok -ql -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: reshape017.f
! %VERIFY: reshape017.out:reshape017.vf
! %STDIN:
! %STDOUT: reshape017.out
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
    type, abstract :: AbstractParent(k1)    ! (4)
        integer, kind :: k1
    end type

    type, extends(AbstractParent) :: Base    ! (4)
        integer(k1) :: i = 88
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) :: j = 99
    end type
end module

program reshape017
use m
    class(AbstractParent(4)), pointer :: b1(:) => null()
    class(*), allocatable :: c1(:,:)
    allocate(b1(10), SOURCE = (/ (Base(4)(i), i=1,10) /))

    allocate(c1(3,5), SOURCE = reshape(b1, (/3,5/), &
     (/Base(4)(-1),Base(4)(-2)/),(/2,1/)))

    select type (b1)
        type is (Base(4))
            print *, b1
        class default
            error stop 1_4
    end select

    select type (c1)
        type is (Base(4))
            print *, c1
        class default
            error stop 2_4
    end select
end
