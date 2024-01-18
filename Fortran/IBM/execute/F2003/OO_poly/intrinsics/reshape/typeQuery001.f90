! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: typeQuery001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
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
!*    Use EXTENDS_TYPE_OF and SAME_TYPE_AS to check the return value.
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
    type Base
        integer :: i = 88
    end type

    type, extends(Base) :: Child
        integer :: j = 99
    end type
end module

program typeQuery001
use m
    class(*), pointer :: b1(:) => null()
    type(Base), pointer :: b2(:) => null()
    class(Base), pointer :: b3(:,:) => null()
    allocate(b1(10), SOURCE = (/ (Child(i,i+100), i=1,10) /))
    allocate(b2(10), SOURCE = (/ (Base(i), i=1,10) /))
    allocate(Child::b3(3,3))

    if(.NOT. same_type_as(reshape(b1, (/3,5/), &
     (/Child(-1,1),Child(-2,2)/), (/1,2/)), Child(1,2))) then
        error stop 1_4
    end if

    if(.NOT. same_type_as(reshape(b2, (/3,5/), &
     (/Base(-1),Base(-2)/), (/1,2/)), Base(1))) then
        error stop 2_4
    end if

    if(.NOT. extends_type_of(reshape(b3, (/3,5/), &
     (/Child(-1,1),Child(-2,2)/), (/2,1/)), Base(1))) then
        error stop 3_4
    end if
end
