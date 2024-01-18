! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qreuse=self /tstdev/F2003/dataPtrAssgn/others/dataPtrComponents1.f
! opt variations: -qnol -qdefaultpv -qreuse=none

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrComponents1.f
!*
!*  PROGRAMMER                 : Michelle Zhang
!*  DATE                       : Aug 31, 2006
!*  ORIGIN                     : Compiler Development, IBM Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION
!*
!* - data-ptr has bounds-spec-list
!* - data-ptr is a component of a derived-type which is the type of a component
!*   of another derived type which contains an allocatable target component
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

   type A(n1,k1)    ! (20,4)
        integer, kind        :: k1
        integer, len         :: n1
        integer(k1), pointer :: p(:)
    end type

    type F(n2,k2)    ! (20,4)
        integer, kind            :: k2
        integer, len             :: n2
        type(A(n2,k2))           :: a1
        integer(k2), allocatable :: tar(:)
    end type

    type(F(20,4)), target :: f1

    allocate(f1%tar(4), source = (/ 1,2,3,4 /))

    f1%a1%p(1:) => f1%tar

    if ( .not. associated(f1%a1%p, f1%tar))stop 1
    if ( any (lbound(f1%a1%p) .ne. (/1/))) stop 2
    if ( any (ubound(f1%a1%p) .ne. (/4/))) stop 3

    print *, f1%a1%p

end program

