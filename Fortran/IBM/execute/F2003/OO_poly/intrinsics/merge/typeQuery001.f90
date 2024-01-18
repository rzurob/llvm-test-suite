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
!*  DATE                       : 01/25/2005
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : merge
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
    class(Base), pointer :: b1
    class(Child), allocatable :: b2(:,:)
    class(*), pointer :: b3(:,:) => null()
    logical :: m1(2,3)

    allocate(b1, SOURCE=Child(7,8))
    allocate(b2(2,3), SOURCE=reshape((/(Child(i,i),i=1,6)/),(/2,3/)))
    allocate(b3(2,3), SOURCE=reshape((/(Child(-i,-i),i=1,6)/),(/2,3/)))
    m1 = reshape((/.FALSE.,.TRUE.,.TRUE.,.FALSE.,.TRUE.,.FALSE./),(/2,3/))

    if(.NOT. same_type_as(merge(b1,b2,.FALSE.), Child(1,1))) error stop 1_4
    if(.NOT. extends_type_of(merge(b2,b1,.FALSE.), Base(1))) error stop 2_4

    if(.NOT. extends_type_of(merge(b3,b2,m1), Base(1))) error stop 3_4
    if(.NOT. same_type_as(merge(b2,b3,m1), merge(b1,b2,.FALSE.))) &
     error stop 4_4
end
