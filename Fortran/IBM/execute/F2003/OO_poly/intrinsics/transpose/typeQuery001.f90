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
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 12/31/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
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
    class(*), pointer :: b1(:,:) => null()
    type(Base), pointer :: b2(:,:) => null()
    class(Base), allocatable :: b3(:,:)

    allocate(Child::b1(3,4))
    allocate(Base::b2(2,2))
    allocate(Child::b3(3,3))

    if(.NOT. same_type_as(transpose(b1), Child(1,2))) error stop 1_4

    if(.NOT. same_type_as(transpose(b2), Base(1))) error stop 2_4

    if(.NOT. same_type_as(transpose(b3), Child(1,2))) error stop 3_4

    if(.NOT. same_type_as(transpose(b1), transpose(b3))) error stop 4_4

    if(.NOT. extends_type_of(transpose(b1), Base(1))) error stop 5_4

    if(extends_type_of(transpose(b2), Child(1,2))) error stop 6_4

    if(.NOT. extends_type_of(transpose(b1), transpose(b2))) then
        error stop 7_4
    end if
end
