! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/typeQuery/definedOperation003.f
! opt variations: -qnock -qnok -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: definedOperation003.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/10/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                : Use defined assignment to test the
!*    dynamic types.
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
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type, extends(Base) :: Child(k2,n2)    ! (4,20,1,10)
        integer, kind             :: k2
        integer, len              :: n2
        character(kind=k2,len=n2) :: c
    end type
end module

program definedOperation003
use m
    interface assignment(=)
        subroutine any2any(a, b)
        use m
            class(AbstractParent(4,*)), intent(out) :: a
            class(AbstractParent(4,*)), intent(in) :: b
        end subroutine
    end interface

    type(Base(4,20)) :: b1 = Base(4,20)(1)
    class(AbstractParent(4,:)), allocatable :: b2
    class(Base(4,:)), pointer :: b3
    type(Child(4,20,1,10)) :: c1 = Child(4,20,1,10)(2, "abc")

    allocate(b2, SOURCE=Base(4,20)(3))
    allocate(b3, SOURCE=Child(4,20,1,10)(4,"def"))

    if(.NOT. same_type_as(b1, Base(4,20)(1))) error stop 1_4
    if(.NOT. same_type_as(b2, Base(4,20)(1))) error stop 2_4
    if(.NOT. same_type_as(b3, Child(4,20,1,10)(1,"a"))) error stop 3_4
    if(.NOT. same_type_as(c1, Child(4,20,1,10)(1,"a"))) error stop 4_4

    b1 = c1
    b3 = b2
    b2 = b1
    c1 = b3

    if(.NOT. same_type_as(b1, Base(4,20)(1))) error stop 5_4
    if(.NOT. same_type_as(b2, Base(4,20)(1))) error stop 6_4
    if(.NOT. same_type_as(b3, Child(4,20,1,10)(1,"a"))) error stop 7_4
    if(.NOT. same_type_as(c1, Child(4,20,1,10)(1,"a"))) error stop 8_4

    if(b1%i /= 2) error stop 9_4

    select type(nameB2=>b2)
        type is (Base(4,*))
            if(nameB2%i /= 2) error stop 10_4
        class default
            error stop 11_4
    end select

    select type(nameB3=>b3)
        type is (Child(4,*,1,*))
            if(nameB3%i /= 3 .OR. nameB3%c /= "c=b") error stop 12_4
        class default
            error stop 13_4
    end select

    if(c1%i /= 3 .OR. c1%c /= "c=b") error stop 14_4
end

subroutine any2any(a, b)
use m
    class(AbstractParent(4,*)), intent(out) :: a
    class(AbstractParent(4,*)), intent(in) :: b
    select type (nameA=>a)
        type is (Child(4,*,1,*))
            select type (nameB=>b)
                type is (Child(4,*,1,*))
                    nameA%i = nameB%i
                    nameA%c = nameB%c
                type is (Base(4,*))
                    nameA%i = nameB%i
                    nameA%c = "c=b"
                class default
                    nameA%i = 888
                    nameA%c = "c=b def"
            end select
        type is (Base(4,*))
            select type (nameB=>b)
                type is (Child(4,*,1,*))
                    nameA%i = nameB%i
                type is (Base(4,*))
                    nameA%i = nameB%i
                class default
                    nameA%i = 999
            end select
        class default
            error stop 15_4
    end select
end subroutine
