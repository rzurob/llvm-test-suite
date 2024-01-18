! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_poly/intrinsics/spread/argAssociation010.f
! opt variations: -ql -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: argAssociation010.f
! %VERIFY: argAssociation010.out:argAssociation010.vf
! %STDIN:
! %STDOUT: argAssociation010.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/20/2005
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is a dummy argument. Dummy argument is a pointer or
!*  allocatable, poly, and is array.
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

program argAssociation010
use m
    class(Base(4)), pointer :: b(:)
    class(Child(4)), allocatable :: c(:,:)

    allocate(b(10), SOURCE=(/(Child(4)(i,i+1),i=1,10)/))
    allocate(c(2,3), SOURCE=reshape((/(Child(4)(i,i+2),i=1,6)/), &
     (/2,3/), (/Child(4)(1,1)/), (/2,1/)))

    call sub1(b, c)

    contains

    subroutine sub1(arg1, arg2)
        class(Base(4)), pointer :: arg1(:)
        class(Child(4)), allocatable :: arg2(:,:)

        select type(name1=>spread(arg1, 2, 3))
            type is (Child(4))
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 1_4
        end select

        select type(name1=>spread(arg2, 3, 2))
            type is (Child(4))
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 2_4
        end select
    end subroutine
end
