! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: functionReturn007.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 11/11/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                : A or MOLD is the return value of a type
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
        character(10) :: c

        contains

        procedure, pass :: clone => cloneChild
    end type

    interface
        function cloneInterface(a)
            import :: AbstractParent
            class(AbstractParent), intent(in) :: a
            class(AbstractParent), allocatable :: cloneInterface
        end function
    end interface

    contains

    function cloneBase(a)
        class(Base), intent(in) :: a
        class(AbstractParent), allocatable :: cloneBase
        allocate(cloneBase, SOURCE=a)
    end function

    function cloneChild(a)
        class(Child), intent(in) :: a
        class(AbstractParent), allocatable :: cloneChild
        allocate(cloneChild, SOURCE=a)
    end function
end module

program functionReturn007
use m
    class(AbstractParent), pointer :: ap1 => null()
    allocate(Base::ap1)

    if(.NOT. extends_type_of(Base(1), ap1%clone())) error stop 1_4
    if(.NOT. extends_type_of(ap1%clone(), Base(1))) error stop 2_4
    if(.NOT. same_type_as(ap1%clone(), Base(1))) error stop 3_4

    deallocate(ap1)
    allocate(Child::ap1)
    if(.NOT. extends_type_of(Child(1,"a"), ap1%clone())) error stop 4_4
    if(.NOT. extends_type_of(ap1%clone(), Child(1,"a"))) error stop 5_4
    if(.NOT. same_type_as(ap1%clone(), Child(1,"a"))) error stop 6_4
end
