! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg014a.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/3/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Argument association (basic test on
!                               assumed-size array dummy-arg)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fArg014a
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,15)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1)    name
    end type

    class (base(4)), allocatable :: b1(:)

    allocate (b1(3), source=(/child(4,1,15)(1,'test1'), child(4,1,15)(2,'test2'), &
        child(4,1,15)(3,'test3')/))


    call abc (b1, (/1,2/))

    call abc (b1(2:3), (/2,3/))

    contains

    subroutine abc (b, i1)
        class (base(4)) b(*)
        integer, intent(in) :: i1(2)

        if (any (b(1:2)%ID /= i1)) error stop 2_4
    end subroutine
    end
