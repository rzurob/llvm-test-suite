! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/mv_Alloc/uLimitPoly/unlimitpoly5.f
! opt variations: -ql -qreuse=self -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/24/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM and TO are unlimit polymorphic, scalar
!*                               keyword FROM/TO provided in a reference to MOVE_ALLOC
!*                               FROM/TO is dummy arg
!*                               FROM/TO has intent, save attribute
!*                               type LOGICAL
!*				 TO is finialized, FROM is not finalized
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

    integer, save :: countA
    integer, save :: countB

    type A(k1,k2)    ! (4,4)
       integer, kind :: k1,k2
       real(k1)         x
       real(k2)         y

       contains
          final :: final1
    end type

    type, extends(A) :: B    ! (4,4)
       real(k1) z

       contains
          final :: final2
    end type

    contains

       subroutine final1(arg)
          type(A(4,4)) :: arg

          countA = countA + 1
          print *, "destroying A object"
       end subroutine

       subroutine final2(arg)
          type(B(4,4)) :: arg

          countB = countB + 1
          print *, "destroying B object"
       end subroutine
end module

program main
use m

    class(*), allocatable :: from
    class(*), asynchronous, allocatable :: to

    ! final subroutines for B and A are called
    allocate(from, source = B(4,4)(8.123, 3.456, 5.003) )

    ! final subroutine for A is called
    allocate(to, source = A(4,4)(10,39) )

    if ( .not. allocated(from) ) stop 21
    if ( .not. allocated(to) ) stop 22

    countB = 0
    countA = 0

    ! final subroutine for A is called
    call move_alloc(from, to)

    if ( countA /= 1 ) stop 23
    if ( countB /= 0 ) stop 24

    if ( allocated(from) ) stop 25
    if ( .not. allocated(to) ) stop 27

    select type(to)
        type is (B(4,4))
            PRINT 10, to
            10  FORMAT(" ", F5.3," ", F5.3, " ", F5.3)
        class default
            stop 33
    end select
end

