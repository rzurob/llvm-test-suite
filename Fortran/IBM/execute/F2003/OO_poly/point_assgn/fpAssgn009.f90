!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn009.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/01/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (if target is unlimted
!*                               polymorphic, then LHS can be sequence type, or
!*                               bind(c) type)
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
    type seq
        sequence
        integer i
    end type
end module


program fpAssgn009
use m
    interface
        logical function test1 (x1, i)
            class (*), intent(in), target :: x1
            integer, intent(in) :: i
        end function
    end interface

    class (*), pointer :: x

    type (seq), target :: seq1
    type (seq), pointer :: seq_ptr

    seq1 = seq(10)

    x => seq1

    seq_ptr => x

    if (seq_ptr%i /= 10) error stop 1_4

    !! sequence type does NOT require use association
    if (.not. test1 (x, 10)) error stop 2_4

    if (.not. test1 (seq_ptr, 10)) error stop 3_4

    if (.not. test1 (seq1, 10)) error stop 4_4
end

logical function test1 (x1, i)
    class (*), intent(in), target :: x1
    integer, intent(in) :: i

    type seq
        sequence
        integer i
    end type

    type(seq), pointer :: s_ptr

    s_ptr => x1

    test1 = (s_ptr%i == i)
end function
