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
    type Base
        integer :: i = -1
    end type

    type Base1
        integer :: j = -2
        integer :: k = -3
        integer :: m = -4
    end type
end module

program functionReturn004
use m
    type(Base) :: b1(3,4)
    class(*), pointer :: b2(:)

    b1 = reshape((/(Base(i), i=1,12)/), (/3,4/))
    allocate(Base1::b2(2))

    select type(name1=>spread(transfer(b1, b2), 2, 2))
        type is (Base1)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
