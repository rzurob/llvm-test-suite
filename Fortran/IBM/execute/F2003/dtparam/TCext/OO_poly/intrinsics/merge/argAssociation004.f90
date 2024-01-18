! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=self -qdefaultpv -qdeferredlp /tstdev/OO_poly/intrinsics/merge/argAssociation004.f
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(k2)    ! (4,4)
        integer, kind :: k2
        integer(k2)      j
    end type
end module

program argAssociation004
use m
    class(Base(4)), pointer :: b(:,:)
    class(Child(4,4)), allocatable :: c(:,:)

    allocate(b(2,3), SOURCE=reshape((/(Child(4,4)(i,-i),i=1,6)/),(/2,3/)))
    allocate(c(2,3), SOURCE=reshape((/(Child(4,4)(i,i+2),i=1,6)/), &
     (/2,3/), (/Child(4,4)(1,1)/), (/2,1/)))

    call sub1(b, c)

    contains

    subroutine sub1(arg1, arg2)
        class(Base(4)), pointer :: arg1(:,:)
        class(Child(4,4)), allocatable :: arg2(:,:)
        logical :: m1(2,3)
        m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE., &
         .FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE./), (/2,3/))

        select type(name1=>merge(arg1, arg2, m1))
            type is (Child(4,4))
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 1_4
        end select
    end subroutine
end
