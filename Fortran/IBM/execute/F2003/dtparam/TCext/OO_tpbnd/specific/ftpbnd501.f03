! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/OO_tpbnd/specific/ftpbnd501.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (nopass binding for
!*                               disassociated pointers and unallocated
!*                               allocatables, poly or nonpoly; test both module
!*                               data and main program data)
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
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains

        procedure, nopass :: print => printBase
    end type

    type, extends (base) :: child    ! (4,20)
        contains

        procedure, nopass ::  print => printChild
    end type

    type (child(4,:)), allocatable :: c1_m(:)
    class (base(4,:)), allocatable :: b1_m
    class (child(4,:)), pointer :: c2_m (:) => null()

    contains

    subroutine printBase
        print *, 'base'
    end subroutine

    subroutine printChild
        print *, 'child'
    end subroutine
end module


program ftpbnd501

use m
    class (base(4,:)), pointer :: b_ptr =>null()

    type (child(4,:)), pointer :: c1

    type p(k2,n2)    ! (4,20)
        integer, kind             :: k2
        integer, len              :: n2
        type(base(k2,:)), pointer :: b1
    end type

    type (p(4,20)) :: p1

    type (child(4,:)), allocatable :: c_alloc(:)
    class (base(4,:)), allocatable :: b_alloc
    class (child(4,:)), allocatable :: c_alloc2

    p1 = p(4,20) (null())
    c1 => null()

    call b_ptr%print
    call p1%b1%print
    call c1%print

    b_ptr => c1
    call b_ptr%print

    call c_alloc%print

    call b_alloc%print

    call c_alloc2%print

    call c1_m%print
    call b1_m%print
    call c2_m%print
    !! the printouts are: base, base, child, base, child, base, child, child,
    !base, child
end
