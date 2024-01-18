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

    type seq1
        sequence
        character*15 :: name
        integer*4 :: id
    end type

    type (seq1) :: s1

    s1 = seq1 (id = 10, name = 's1')

    call printSeq1 (s1)
end

subroutine printSeq1 (x)
    class (*), intent(in), target :: x

    type seq1
        sequence
        character*15 :: name
        integer*4 :: id
    end type

    type (seq1), pointer :: s1

    s1 => x

    print *, s1
end subroutine
