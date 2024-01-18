! GB DTP extension using:
! ftcx_dtp -ql -qreuse=none /tstdev/OO_poly/intrinsics/reshape/reshape019.f
! opt variations: -qnol -qreuse=base

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: reshape019.f
! %VERIFY: reshape019.out:reshape019.vf
! %STDIN:
! %STDOUT: reshape019.out
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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: i = 88
    end type

    type, extends(Base) :: Child(n2,k2)    ! (20,4,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)   :: j = 99
    end type
end module

program reshape019
use m
    class(*), pointer :: b1(:) => null()
    class(*), allocatable :: c1(:,:)
    allocate(b1(20), SOURCE = (/ (Child(20,4,20,4)(i,i+100), i=1,20) /))

    allocate(c1(3,5), SOURCE = reshape(b1, (/3,5/)))

    select type (b1)
        type is (Child(*,4,*,4))
            print *, b1
        class default
            error stop 1_4
    end select

    select type (c1)
        type is (Child(*,4,*,4))
            print *, c1
        class default
            error stop 2_4
    end select
end
