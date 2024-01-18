! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/intrinsics/spread/functionReturn004.f
! opt variations: -ql -qreuse=self

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: functionReturn004.f
! %VERIFY: functionReturn004.out:functionReturn004.vf
! %STDIN:
! %STDOUT: functionReturn004.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/18/2005
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is the return value of intrinsic function transfer.
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
        integer(k1)   :: i = -1
    end type

    type Base1(k2,k3,k4)    ! (4,4,4)
        integer, kind :: k2,k3,k4
        integer(k2)   :: j = -2
        integer(k3)   :: k = -3
        integer(k4)   :: m = -4
    end type
end module

program functionReturn004
use m
    type(Base(4)) :: b1(3,4)
    class(*), pointer :: b2(:)

    b1 = reshape((/(Base(4)(i), i=1,12)/), (/3,4/))
    allocate(Base1(4,4,4)::b2(2))

    select type(name1=>spread(transfer(b1, b2), 2, 2))
        type is (Base1(4,4,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
