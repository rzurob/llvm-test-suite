! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc019a.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc019a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (the target allocated via allocation
!                               of a pointer has the TARGET attribute; so are
!                               the sub-objects of it)
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
    type seq1(k1,k2)    ! (4,8)
        integer, kind :: k1,k2
        sequence
        integer(k1)   :: i1, i2
        integer(k2)   :: i3
    end type

    type base(k3,k4)    ! (4,8)
        integer, kind     :: k3,k4
        type(seq1(k3,k4)) :: data(2) = seq1(k3,k4) (1, 10, i3 = 100_8)
    end type
end module

program falloc019a
use m
    class (base(4,8)), pointer :: b1_ptr

    class (*), pointer :: x, x1(:)
    type (seq1(4,8)), pointer :: s, s1(:)

    allocate (b1_ptr)

    x => b1_ptr%data(1)

    x1 => b1_ptr%data

    s => x
    s1 => x1

    if ((s%i1 /= 1) .or. (s%i2 /= 10) .or. (s%i3 /= 100_8)) error stop 1_4

    if ((s1(1)%i1 /= 1) .or. (s1(1)%i2 /= 10) .or. &
        (s1(1)%i3 /= 100_8)) error stop 2_4

    if ((s1(2)%i1 /= 1) .or. (s1(2)%i2 /= 10) .or. &
        (s1(2)%i3 /= 100_8)) error stop 2_4

end
