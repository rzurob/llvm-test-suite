! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: type002.f
! %VERIFY: type002.out:type002.vf
! %STDIN:
! %STDOUT: type002.out
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
!*    Declared types of TSOURCE and FSOURCE are different, but their
!*  dynamic types are the same.
!*    TSOURCE is scalar or array
!*    FSOURCE is scalar or array
!*    MASK is scalar or array
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
        integer :: i = 8
    end type

    type, extends(Base) :: Child
        integer :: j = 9
    end type
end module

program type002
use m
    class(Base), pointer :: b1(:,:,:)
    class(Child), allocatable :: b2

    allocate(b1(2,2,2), SOURCE=reshape((/(Child(i,-i),i=1,8)/), &
     (/2,2,2/)))
    allocate(b2, SOURCE=Child(88,99))

    select type(name1=>merge(b1, b2, .TRUE.))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    select type(name1=>merge(b2, b1, reshape((/.TRUE.,.FALSE., &
     .TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE./),(/2,2,2/))))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
