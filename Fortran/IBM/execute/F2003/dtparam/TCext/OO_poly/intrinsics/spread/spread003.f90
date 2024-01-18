! GB DTP extension using:
! ftcx_dtp -ql -qreuse=base /tstdev/OO_poly/intrinsics/spread/spread003.f
! opt variations: -qnol -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: spread003.f
! %VERIFY: spread003.out:spread003.vf
! %STDIN:
! %STDOUT: spread003.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/05/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    The return is a zero-sized array if NCOPIES is 0.
!*    Poly.
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
        integer(k1)   :: i = 8
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) :: j = 9
    end type
end module

program spread003
use m
    class(*), allocatable :: b1(:,:)
    allocate(b1(2,3), SOURCE=reshape((/(Child(20,4)(i,i),i=1,6)/), (/2,3/)))

    select type(name1=>spread(b1, 1, 0))
        type is (Child(*,4))
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    select type(name1=>spread(b1, 2, 0))
        type is (Child(*,4))
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select

    select type(name1=>spread(b1, 3, 0))
        type is (Child(*,4))
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 3_4
    end select
end
