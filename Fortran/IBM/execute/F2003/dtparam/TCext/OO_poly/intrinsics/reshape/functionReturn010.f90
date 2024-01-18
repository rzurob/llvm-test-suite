! GB DTP extension using:
! ftcx_dtp -qck -qk -qnol -qreuse=base /tstdev/OO_poly/intrinsics/reshape/functionReturn010.f
! opt variations: -qnock -qnok -ql -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/11/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                : SOURCE is the return value of a type
!*    bound procedure call. Use abstract type and deferred bounding.
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
    type, abstract :: AbstractParent(k1)    ! (4)
        integer, kind :: k1
        contains

        procedure(cloneInterface), pass, deferred :: clone
    end type

    type, extends(AbstractParent) :: Base    ! (4)
        integer(k1) i

        contains

        procedure, pass :: clone => cloneBase
    end type

    type, extends(Base) :: Child(k2,n1)    ! (4,1,3)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: c

        contains

        procedure, pass :: clone => cloneChild
    end type

    interface
        function cloneInterface(a, i)
            import :: AbstractParent
            class(AbstractParent(4)), intent(in) :: a
            integer, intent(in) :: i
            class(AbstractParent(4)), allocatable :: cloneInterface(:)
        end function
    end interface

    contains

    function cloneBase(a, i)
        class(Base(4)), intent(in) :: a
        integer, intent(in) :: i
        class(AbstractParent(4)), allocatable :: cloneBase(:)
        allocate(cloneBase(i), SOURCE=a)
    end function

    function cloneChild(a, i)
        class(Child(4,1,*)), intent(in) :: a
        integer, intent(in) :: i
        class(AbstractParent(4)), allocatable :: cloneChild(:)
        allocate(cloneChild(i), SOURCE=a)
    end function
end module

program functionReturn010
use m
    class(AbstractParent(4)), pointer :: ap1 => null()
    class(AbstractParent(4)), allocatable :: ap2(:,:)

    allocate(ap1, SOURCE=Base(4)(3))
    allocate(ap2(3,5), SOURCE=reshape(ap1%clone(20), (/3,5/)))

    select type (ap2)
        type is (Base(4))
            print *, ap2
            print *, size(ap2)
            print *, shape(ap2)
        class default
            error stop 1_4
    end select

    deallocate(ap1, ap2)

    allocate(ap1, SOURCE=Child(4,1,3)(4, "abc"))
    allocate(ap2(5,4), SOURCE=reshape(ap1%clone(20), (/5,4/)))

    select type (ap2)
        type is (Child(4,1,*))
            print *, ap2
            print *, size(ap2)
            print *, shape(ap2)
        class default
            error stop 2_4
    end select
end
