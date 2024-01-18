!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn024.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (non-poly pointer array
!*                               assigned to poly-pointer array; TARGET is the
!*                               parent component; use rank-one arrays)
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
    type base
        integer*4 :: id = 0
    end type

    type, extends (base) :: child
        character*20 :: name = ''
    end type

    type (child), target, allocatable :: c1_m(:)
end module

program fpAssgn024
use m
    type (child), target :: c1(10)
    type (child), target :: c2(3,3)

    class (base), pointer :: b_ptr(:)
    type (base), pointer :: b1(:)

    allocate (c1_m(20))

    c1_m%id = (/(i+10, i=1, 20)/)
    c1_m%name = 'c1_m'

    c1 = (/(child(i,name='c1'), i=1,10)/)

    c2 = reshape ((/(child(i, name='c2'),i=31,39)/), (/3,3/))

    !! transfer c1_m%base to b1 through poly-pointer
    b_ptr => c1_m
    b1 => b_ptr

    if ((size (b1) /= 20) .or. (.not. associated (b1, c1_m%base))) error stop 1_4

    if (any (b1%id /= (/(i,i=11,30)/))) error stop 2_4

    !! transfer c1(::2)%base to b1 through poly-pointer
    b_ptr => c1

    b1 => b_ptr(::2)

    if ((size(b1) /= 5) .or. (.not. associated(b1, c1(::2)%base))) error stop 3_4

    if (any (b1%id /= (/(i,i=1,10,2)/))) error stop 4_4

    !! transfer c2(2,::2)%base to b1 through poly-pointer
    b_ptr => c2 (2, :)

    b1 => b_ptr (::2)

    if ((size(b1) /= 2) .or. (.not. associated (b1, c2(2,::2)%base))) error stop 5_4

    if ((b1(1)%id /= 32) .or. (b1(2)%id /= 38)) error stop 6_4
end
