! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: functionReturn010.f
! %VERIFY: functionReturn010.out:functionReturn010.vf
! %STDIN:
! %STDOUT: functionReturn010.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 02/20/2006
!*  ORIGIN                     :
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
    type, abstract :: AbstractParent
        contains

        procedure(cloneInterface), pass, deferred :: clone
    end type

    type, extends(AbstractParent) :: Base
        integer i

        contains

        procedure, pass :: clone => cloneBase
    end type

    type, extends(Base) :: Child
        character(3) :: c

        contains

        procedure, pass :: clone => cloneChild
    end type

    abstract interface
        function cloneInterface(a, i)
            import :: AbstractParent
            class(AbstractParent), intent(in) :: a
            integer, intent(in) :: i
            class(AbstractParent), allocatable :: cloneInterface(:)
        end function
    end interface

    contains

    function cloneBase(a, i)
        class(Base), intent(in) :: a
        integer, intent(in) :: i
        class(AbstractParent), allocatable :: cloneBase(:)
        allocate(cloneBase(i), SOURCE=a)
    end function

    function cloneChild(a, i)
        class(Child), intent(in) :: a
        integer, intent(in) :: i
        class(AbstractParent), allocatable :: cloneChild(:)
        allocate(cloneChild(i), SOURCE=a)
    end function
end module

program abstracti014
use m
    class(AbstractParent), pointer :: ap1 => null()
    class(AbstractParent), allocatable :: ap2(:,:)

    allocate(ap1, SOURCE=Base(3))
    allocate(ap2(3,5), SOURCE=reshape(ap1%clone(20), (/3,5/)))

    select type (ap2)
        type is (Base)
            print *, ap2
            print *, size(ap2)
            print *, shape(ap2)
        class default
            error stop 1_4
    end select

    deallocate(ap1, ap2)

    allocate(ap1, SOURCE=Child(4, "abc"))
    allocate(ap2(5,4), SOURCE=reshape(ap1%clone(20), (/5,4/)))

    select type (ap2)
        type is (Child)
            print *, ap2
            print *, size(ap2)
            print *, shape(ap2)
        class default
            error stop 2_4
    end select
end
