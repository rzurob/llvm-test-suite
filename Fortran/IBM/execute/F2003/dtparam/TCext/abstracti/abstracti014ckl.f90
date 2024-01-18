! GM DTP extension using:
! ftcx_dtp -qck -qk -ql -qnodeferredlp -qreuse=none /tstdev/F2003/abstracti/functional/abstracti014.f

!************************************************************************
!* ======================================================================
!*  TEST CASE NAME             : abstracti014ckl
!*                               by Alberto Alvarez-Mesquida)
!*  DATE                       : 2007-10-24 (original: 02/20/2006)
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : reshape
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*  DESCRIPTION                : SOURCE is the return value of a type
!*  bound procedure call. Use abstract type and deferred bounding.
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY :
!*  Init :
!*  Comments :
!*  ===================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module m
    type, abstract :: AbstractParent(k1,l1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: l1
        contains

        procedure(cloneInterface), pass, deferred :: clone
    end type

    type, extends(AbstractParent) :: Base(l2,k2)    ! (4,20,20,4)
        integer, kind :: k2
        integer, len  :: l2
        integer(k2)      i

        contains

        procedure, pass :: clone => cloneBase
    end type

    type, extends(Base) :: Child(k3,l3)    ! (4,20,20,4,1,3)
        integer, kind             :: k3
        integer, len              :: l3
        character(kind=k3,len=l3) :: c

        contains

        procedure, pass :: clone => cloneChild
    end type

    abstract interface
        function cloneInterface(a, i)
            import :: AbstractParent
            class(AbstractParent(4,*)), intent(in) :: a
            integer, intent(in) :: i
            class(AbstractParent(4,20)), allocatable :: cloneInterface(:)
        end function
    end interface

    contains

    function cloneBase(a, i)
        class(Base(4,*,*,4)), intent(in) :: a
        integer, intent(in) :: i
        class(AbstractParent(4,20)), allocatable :: cloneBase(:)
        allocate(cloneBase(i), SOURCE=a)
    end function

    function cloneChild(a, i)
        class(Child(4,*,*,4,1,*)), intent(in) :: a
        integer, intent(in) :: i
        class(AbstractParent(4,20)), allocatable :: cloneChild(:)
        allocate(cloneChild(i), SOURCE=a)
    end function
end module

program abstracti014ckl
use m
    class(AbstractParent(4,20)), pointer :: ap1 => null()
    class(AbstractParent(4,20)), allocatable :: ap2(:,:)

    allocate(ap1, SOURCE=Base(4,20,20,4)(3))
    allocate(ap2(3,5), SOURCE=reshape(ap1%clone(20), (/3,5/)))

    select type (ap2)
        type is (Base(4,*,*,4))
            print *, ap2
            print *, size(ap2)
            print *, shape(ap2)
        class default
            error stop 1_4
    end select

    deallocate(ap1, ap2)

    allocate(ap1, SOURCE=Child(4,20,20,4,1,3)(4, "abc"))
    allocate(ap2(5,4), SOURCE=reshape(ap1%clone(20), (/5,4/)))

    select type (ap2)
        type is (Child(4,*,*,4,1,*))
            print *, ap2
            print *, size(ap2)
            print *, shape(ap2)
        class default
            error stop 2_4
    end select
end
