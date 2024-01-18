! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: argAssociation004.f
! %VERIFY: argAssociation004.out:argAssociation004.vf
! %STDIN:
! %STDOUT: argAssociation004.out
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
!*    TSOURCE or FSOURCE is a dummy argument. Dummy argument is a
!*  pointer or allocatable, poly, and is array.
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

    type, extends(Base) :: Child
        integer j
    end type
end module

program argAssociation004
use m
    class(Base), pointer :: b(:,:)
    class(Child), allocatable :: c(:,:)

    allocate(b(2,3), SOURCE=reshape((/(Child(i,-i),i=1,6)/),(/2,3/)))
    allocate(c(2,3), SOURCE=reshape((/(Child(i,i+2),i=1,6)/), &
     (/2,3/), (/Child(1,1)/), (/2,1/)))

    call sub1(b, c)

    contains

    subroutine sub1(arg1, arg2)
        class(Base), pointer :: arg1(:,:)
        class(Child), allocatable :: arg2(:,:)
        logical :: m1(2,3)
        m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE., &
         .FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE./), (/2,3/))

        select type(name1=>merge(arg1, arg2, m1))
            type is (Child)
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 1_4
        end select
    end subroutine
end
