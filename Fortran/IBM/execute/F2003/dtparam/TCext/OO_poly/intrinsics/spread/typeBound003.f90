! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/spread/typeBound003.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: typeBound003.f
! %VERIFY: typeBound003.out:typeBound003.vf
! %STDIN:
! %STDOUT: typeBound003.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/18/2005
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Cross testing type bound.
!*    Polymorphic
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
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains
        procedure :: spreadMe
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4,20)
        integer(k1) j
    end type

    contains

    function spreadMe(this, i)
        class(AbstractParent(4,*)), intent(in) :: this
        integer, intent(in) :: i
        class(AbstractParent(4,:)), pointer :: spreadMe(:)
        allocate(spreadMe(i), SOURCE=spread(this, 1, i))
    end function
end module

program typeBound003
use m
    class(*), allocatable :: b1
    allocate(b1, SOURCE=Base(4,20)(2))

    select type(b1)
        type is (Base(4,*))
            select type(name1=>b1%spreadMe(4))
                type is (Base(4,*))
                    print *, "Base", name1
                    print *, size(name1)
                    print *, shape(name1)
                class default
                    error stop 1_4
            end select
        class default
            error stop 2_4
    end select

    deallocate(b1)
    allocate(b1, SOURCE=Child(4,20)(3,4))

    select type(b1)
        type is (Child(4,*))
            select type(name1=>b1%spreadMe(8))
                type is (Child(4,*))
                    print *, "Child", name1
                    print *, size(name1)
                    print *, shape(name1)
                class default
                    error stop 3_4
            end select
        class default
            error stop 4_4
    end select
end
