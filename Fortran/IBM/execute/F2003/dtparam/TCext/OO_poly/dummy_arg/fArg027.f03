! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg027.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/04/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (assumed-shape array used
!                               as the actual argument to an assumed-size array)
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

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    private internalT

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine test1 (b)
        class (base(4)), intent(inout) :: b (:,:)

        call interalT (b)
    end subroutine

    subroutine interalT (b)
        type (base(4)), intent(inout) :: b(*)

        b(2:4:2)%id = b(2:4:2)%id *2
    end subroutine
end module

program fArg027
use m
    class (base(4)), allocatable :: b1(:,:)

    type (child(4,1,20)) :: c1 (2,2)

    !allocate (b1(2,2), source=reshape ((/child(1,'b1_1'), child(2,'b1_2'), &
    !                child(3, 'b1_3'), child(4,'b1_4')/), (/2,2/)))


    !
    c1 =reshape ((/child(4,1,20)(1,'b1_1'), child(4,1,20)(2,'b1_2'), &
                child(4,1,20)(3, 'b1_3'), child(4,1,20)(4,'b1_4')/), (/2,2/))
    !

    call test1 (c1)

    call c1(1,1)%print
    call c1(2,1)%print
    call c1(1,2)%print
    call c1(2,2)%print
end
