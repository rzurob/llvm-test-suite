! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/merge/argAssociation005.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: argAssociation005.f
! %VERIFY: argAssociation005.out:argAssociation005.vf
! %STDIN:
! %STDOUT: argAssociation005.out
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
!*  pointer or allocatable, unlimited poly, and is array.
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type
end module

program argAssociation005
use m
    class(*), pointer :: b(:,:)
    class(*), allocatable :: c(:,:)

    allocate(b(2,3), SOURCE=reshape((/(Child(4)(i,-i),i=1,6)/),(/2,3/)))
    allocate(c(2,3), SOURCE=reshape((/(Child(4)(i,i+2),i=1,6)/), &
     (/2,3/), (/Child(4)(1,1)/), (/2,1/)))

    call sub1(b, c)

    contains

    subroutine sub1(arg1, arg2)
        class(*), pointer :: arg1(:,:)
        class(*), allocatable :: arg2(:,:)
        logical :: m1(2,3)
        m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE., &
         .FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE./), (/2,3/))

        select type(name1=>merge(arg1, arg2, m1))
            type is (Child(4))
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 1_4
        end select
    end subroutine
end
