! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/typeQuery/associate001.f
! opt variations: -qck -qnok -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: associate001.f
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
!*  DATE                       : 10/25/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                : Query type inside an associate construct.
!*    Selector is scalar.
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

    type, extends(Base) :: Child(n2)    ! (4,20,10)
        integer, len  :: n2
        character(n2) :: c
    end type
end module

program associate001
use m
    class(*), pointer :: i => null()
    class(*), pointer :: ap1 => null()
    class(AbstractParent(4,:)), allocatable :: ap2

    allocate(Base(4,20)::ap1)

    associate(name1=>ap1, name2=>ap2)
        if(.NOT. extends_type_of(name1, name2)) error stop 1_4
        if(.NOT. extends_type_of(name1, ap2)) error stop 2_4
        if(.NOT. extends_type_of(name1, i)) error stop 3_4
        if(.NOT. extends_type_of(name2, i)) error stop 4_4
        if(same_type_as(name1, name2)) error stop 5_4
        if(.NOT. same_type_as(name1, ap1)) error stop 6_4
        if(.NOT. same_type_as(name2, ap2)) error stop 7_4
        if(same_type_as(name1, i)) error stop 8_4
    end associate

    allocate(Child(4,20,10)::ap2)
    allocate(integer::i)

    associate(name1=>ap1, name2=>ap2)
        if(.NOT. extends_type_of(name2, name1)) error stop 9_4
        if(.NOT. extends_type_of(name2, ap1)) error stop 10_4
        if(same_type_as(name2, name1)) error stop 11_4
        if(.NOT. same_type_as(name1, ap1)) error stop 12_4
        if(.NOT. same_type_as(name2, ap2)) error stop 13_4
    end associate
end
