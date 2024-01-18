! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: argumentKeyword001.f
! %VERIFY: argumentKeyword001.out:argumentKeyword001.vf
! %STDIN:
! %STDOUT: argumentKeyword001.out
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
!*    Actual arguments are specified using argument keywords.
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
        integer i
    end type
end module

program argumentKeyword001
use m
    class(Base), pointer :: b1(:,:)
    class(*), allocatable :: b2(:,:)
    logical :: m1(2,3)

    allocate(b1(2,3), SOURCE=reshape((/(Base(i),i=1,6)/), (/2,3/)))
    allocate(b2(2,3), SOURCE=reshape((/(Base(-i),i=1,6)/), (/2,3/)))
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE./),(/2,3/))

    select type(name1=>merge(FSOURCE=b1, TSOURCE=b2, MASK=m1))
        type is(Base)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
