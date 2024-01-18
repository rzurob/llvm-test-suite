! GB DTP extension using:
! ftcx_dtp -ql -qreuse=none /tstdev/F2003/dataPtrAssgn/others/dataPtrComponents.f
! opt variations: -qnol -qreuse=base

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrComponents.f
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
!* - data-ptr has bounds-remapping-list
!* - data-ptr and data-tar are components of two different derived-type,
!*   where one derived-type is an extended DT from the other and the parent
!*   DT contains the pointer component
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

    type :: D(n1,k1)    ! (20,4)
        integer, kind        :: k1
        integer, len         :: n1
        integer(k1), pointer :: p(:,:,:)
    end type

    type, extends(D) :: E(n2,k2)    ! (20,4,20,4)
        integer, kind            :: k2
        integer, len             :: n2
        integer(k2), allocatable :: tar(:)
    end type

    type(E(20,4,20,4)), target :: e1

    allocate(e1%tar(3), source = (/ 1,2,3 /) )

    e1%p(3:3,0:0,4:6) => e1%tar

    if ( .not. associated(e1%p)) stop 1
    if ( any (lbound(e1%p) .ne. (/3,0,4/))) stop 3
    if ( any (ubound(e1%p) .ne. (/3,0,6/))) stop 5

    print *, e1%p

end program
