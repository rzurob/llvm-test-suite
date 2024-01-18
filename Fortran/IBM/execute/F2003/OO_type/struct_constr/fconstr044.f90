!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr044.f
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
!*  DESCRIPTION                : structure constructor (sequence pointer
!*                               component initialization in struct_constr using
!*                               class(*) target)
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
    type seq1
        sequence
        integer*2 :: i1
        integer*4 :: i2
    end type

    type base
        integer*4 :: id = 0
        type (seq1), pointer :: s1 => null()
    end type
end module

program fconstr044
use m
    type (base) :: b1

    class (*), pointer :: x

    nullify (x)

    b1 = base (id = 10, s1 = x)

    if (associated (b1%s1)) error stop 1_4

    call assgnS1 (b1)

    if (b1%id /= 10) error stop 2_4

    if (.not. associated (b1%s1)) error stop 3_4

    if ((b1%s1%i1 /= 10) .or. (b1%s1%i2 /= 100)) error stop 4_4
end

subroutine assgnS1 (b1)
use m, only: base
    type (base), intent(inout) :: b1

    class (*), pointer :: x

    type seq1
        sequence
        integer*2 :: i1
        integer*4 :: i2
    end type

    type (seq1), target :: s11 = seq1 (10, 100)

    x => s11

    b1 = base (b1%id, x)
end subroutine
