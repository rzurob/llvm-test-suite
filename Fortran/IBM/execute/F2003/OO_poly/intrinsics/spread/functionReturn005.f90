! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: functionReturn005.f
! %VERIFY: functionReturn005.out:functionReturn005.vf
! %STDIN:
! %STDOUT: functionReturn005.out
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
!*    SOURCE is the return value of intrinsic function transpose.
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

    type, extends(Base) :: Child
        integer :: j = -2
    end type
end module

program functionReturn005
use m
    class(Base), pointer :: b1(:)
    class(*), allocatable :: b2(:,:,:)

    allocate(b1(10), SOURCE=(/(Base(i),i=1,10)/))

    select type(name1=>spread(transpose(reshape(b1,(/3,5/), &
     (/Base(-1),Base(-2)/),(/2,1/))), 3, 2))
        type is (Base)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    allocate(b2(2,2,2), SOURCE=reshape((/(Child(i,-i),i=1,8)/),(/2,2,2/)))

    select type(name1=>spread(transpose(reshape(b2,(/4,2/))),3,3))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
