! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/typeQuery/functionReturn007.f
! opt variations: -qck -qnok -qnol -qnodeferredlp -qreuse=none

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
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains

        procedure(cloneInterface), pass, deferred :: clone
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i

        contains

        procedure, pass :: clone => cloneBase
    end type

    type, extends(Base) :: Child(n2)    ! (4,20,10)
        integer, len  :: n2
        character(n2) :: c

        contains

        procedure, pass :: clone => cloneChild
    end type

    interface
        function cloneInterface(a)
            import :: AbstractParent
            class(AbstractParent(4,*)), intent(in) :: a
            class(AbstractParent(4,:)), allocatable :: cloneInterface
        end function
    end interface

    contains

    function cloneBase(a)
        class(Base(4,*)), intent(in) :: a
        class(AbstractParent(4,:)), allocatable :: cloneBase
        allocate(cloneBase, SOURCE=a)
    end function

    function cloneChild(a)
        class(Child(4,*,*)), intent(in) :: a
        class(AbstractParent(4,:)), allocatable :: cloneChild
        allocate(cloneChild, SOURCE=a)
    end function
end module

program functionReturn007
use m
    class(AbstractParent(4,:)), pointer :: ap1 => null()
    allocate(Base(4,20)::ap1)

    if(.NOT. extends_type_of(Base(4,20)(1), ap1%clone())) error stop 1_4
    if(.NOT. extends_type_of(ap1%clone(), Base(4,20)(1))) error stop 2_4
    if(.NOT. same_type_as(ap1%clone(), Base(4,20)(1))) error stop 3_4

    deallocate(ap1)
    allocate(Child(4,20,10)::ap1)
    if(.NOT. extends_type_of(Child(4,20,10)(1,"a"), ap1%clone())) error stop 4_4
    if(.NOT. extends_type_of(ap1%clone(), Child(4,20,10)(1,"a"))) error stop 5_4
    if(.NOT. same_type_as(ap1%clone(), Child(4,20,10)(1,"a"))) error stop 6_4
end
