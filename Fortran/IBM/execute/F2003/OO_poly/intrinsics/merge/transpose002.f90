! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: transpose002.f
! %VERIFY: transpose002.out:transpose002.vf
! %STDIN:
! %STDOUT: transpose002.out
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
!*    Function return of merge is MATRIX of transpose.
!*    Poly and unlimited poly.
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
    type, abstract :: AbstractParent
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

program transpose002
use m
    class(*), pointer :: b1(:,:)
    class(AbstractParent), pointer :: c1(:,:)
    logical :: m1(2,4)

    allocate(b1(2,4), SOURCE=reshape((/(Child(i,-i),i=1,8)/),(/2,4/)))
    allocate(c1(2,4), SOURCE=reshape((/(Child(i,i-1),i=1,8)/),(/2,4/)))
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.TRUE.,.FALSE., &
     .TRUE.,.FALSE.,.FALSE./), (/2,4/))

    select type(name1=>transpose(merge(b1, c1, m1)))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
