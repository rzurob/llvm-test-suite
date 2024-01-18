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
!*  DATE                       : 12/30/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE/MOLD is the return value of intrinsic function transpose.
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
    type(Base) :: b1(10)
    class(*), pointer :: b2(:)

    b1 = (/ (Base(i), i=1,10) /)
    allocate(Base1::b2(2))

    select type(name1=>transfer(transpose(reshape(b1,(/3,5/), &
     (/Base(-1),Base(-2)/),(/2,1/))), b2))
        type is (Base1)
            if(size(name1) .NE. 5) error stop 1_4
            print *, name1
        class default
            error stop 2_4
    end select
end
