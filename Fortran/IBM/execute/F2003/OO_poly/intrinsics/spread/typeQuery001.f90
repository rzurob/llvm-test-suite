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
!*  DATE                       : 01/06/2005
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : spread
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
    class(Base), pointer :: b1
    class(Base), allocatable :: b2(:,:)
    class(*), pointer :: b3(:) => null()

    allocate(b1, SOURCE=Child(1,2))
    allocate(b2(2,3), SOURCE=reshape((/(Child(i,i),i=1,6)/), (/2,3/)))
    allocate(Child::b3(3))

    if(.NOT. same_type_as(spread(b1,1,2), Child(1,1))) error stop 1_4
    if(.NOT. extends_type_of(spread(b1,1,2), Base(1))) error stop 2_4

    if(.NOT. same_type_as(spread(b2,3,2), Child(1,1))) error stop 3_4
    if(.NOT. extends_type_of(spread(b2,3,2), Base(1))) error stop 4_4

    if(.NOT. same_type_as(spread(b3,2,2), Child(1,1))) error stop 5_4
    if(.NOT. extends_type_of(spread(b3,2,2), Base(1))) error stop 6_4

    if(.NOT. same_type_as(spread(b1,1,2), spread(b2,3,2))) error stop 7_4

    if(.NOT. same_type_as(spread(b2,1,2), spread(b3,1,2))) error stop 8_4
end
