! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: type003.f
! %VERIFY: type003.out:type003.vf
! %STDIN:
! %STDOUT: type003.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 01/25/2005
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : merge
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
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

program type003
use m
    class(Base), pointer :: b1(:,:)
    class(*), allocatable :: b2(:,:)
    logical :: m1(3,2)

    allocate(b1(3,2), SOURCE=reshape((/(Base(i),i=1,6)/),(/3,2/)))
    allocate(b2(3,2), SOURCE=reshape((/(Base(i),i=101,106)/),(/3,2/)))
    m1 = reshape((/.TRUE.,.FALSE., &
     .TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE./),(/3,2/))

    select type(name1=>merge(b1, b2, m1))
        type is (Base)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    deallocate(b1, b2)
    allocate(b1(3,2), SOURCE=reshape((/(Child(i,-i),i=1,6)/),(/3,2/)))
    allocate(b2(3,2), SOURCE=reshape((/(Child(i,-i),i=101,106)/),(/3,2/)))

    select type(name1=>merge(b2, b1, m1))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
