! GB DTP extension using:
! ftcx_dtp -ql -qreuse=none /tstdev/OO_poly/intrinsics/spread/functionReturn006.f
! opt variations: -qnol -qreuse=base

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: functionReturn006.f
! %VERIFY: functionReturn006.out:functionReturn006.vf
! %STDIN:
! %STDOUT: functionReturn006.out
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
!*    SOURCE is the return value of intrinsic function spread.
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
        integer(k1)   :: i = -1
    end type

    type, extends(Base) :: Child(n2,k2)    ! (20,4,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)   :: j = -2
    end type
end module

program functionReturn006
use m
    class(*), allocatable :: b1(:,:,:)

    allocate(b1(2,2,2), SOURCE=reshape((/(Child(20,4,20,4)(i,-i),i=1,8)/),(/2,2,2/)))

    select type(name1=>spread(spread(b1,4,3), 5, 2))
        type is (Child(*,4,*,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
