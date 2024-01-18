! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: functionReturn003.f
! %VERIFY: functionReturn003.out:functionReturn003.vf
! %STDIN:
! %STDOUT: functionReturn003.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 01/18/2005
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    SOURCE is the return value of intrinsic function reshape().
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
        integer :: i = 1
    end type

    type, extends(Base) :: Child
        integer :: j = 2
    end type
end module

program functionReturn003
use m
    class(*), pointer :: b1(:)
    class(Base), allocatable :: b2(:,:)

    allocate(b1(10), SOURCE=(/(Base(i),i=-1,-10,-1)/))

    select type(name1=>spread(reshape(b1,(/3,3/)), 3, 2))
        type is (Base)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    allocate(b2(3,4), SOURCE=reshape((/(Child(i,-i),i=1,12)/), (/3,4/)))

    select type(name1=>spread(reshape(b2,(/2,2,2/)), 4, 2))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
