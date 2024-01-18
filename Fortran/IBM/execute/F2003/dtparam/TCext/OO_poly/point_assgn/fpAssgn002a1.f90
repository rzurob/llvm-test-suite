! GB DTP extension using:
! ftcx_dtp -qck -qdeferredlp /tstdev/OO_poly/point_assgn/fpAssgn002a1.f
! opt variations: -qnock -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (LHS can be sequence
!*                               type if RHS is an unlimited poly target; test
!*                               this in a procedure call)
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

program fpAssgn002a1
    interface
        subroutine printSeq1 (x)
            class (*), intent(in), target :: x
        end subroutine
    end interface

    type seq1(k1,n1,k2)    ! (1,15,4)
        integer, kind             :: k1,k2
        integer, len              :: n1
        sequence
        character(kind=k1,len=n1) :: name
        integer(k2)               :: id
    end type

    type (seq1(1,15,4)) :: s1

    s1 = seq1(1,15,4) (id = 10, name = 's1')

    call printSeq1 (s1)
end

subroutine printSeq1 (x)
    class (*), intent(in), target :: x

    type seq1(k3,n2,k4)    ! (1,15,4)
        integer, kind             :: k3,k4
        integer, len              :: n2
        sequence
        character(kind=k3,len=n2) :: name
        integer(k4)               :: id
    end type

    type (seq1(1,:,4)), pointer :: s1

    s1 => x

    print *, s1
end subroutine
