! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_poly/intrinsics/reshape/reshape021.f
! opt variations: -ql -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: reshape021.f
! %VERIFY: reshape021.out:reshape021.vf
! %STDIN:
! %STDOUT: reshape021.out
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
!*    SOURCE is unlimited poly
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: i = 88
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) :: j = 99
    end type
end module

program reshape021
use m
    class(*), pointer :: b1(:) => null()
    class(*), allocatable :: c1(:,:)
    class(*), allocatable :: c2(:)

    allocate(b1(10), SOURCE = (/ (Child(4)(i,i+100), i=1,10) /))
    allocate(c2(2), SOURCE = (/ Child(4)(-1,1),Child(4)(-2,2) /))
    allocate(c1(3,5), SOURCE = reshape(b1, (/3,5/), c2, (/1,2/)))

    select type (b1)
        type is (Child(4))
            print *, b1
        class default
            error stop 1_4
    end select

    select type (c1)
        type is (Child(4))
            print *, c1
        class default
            error stop 2_4
    end select
end
