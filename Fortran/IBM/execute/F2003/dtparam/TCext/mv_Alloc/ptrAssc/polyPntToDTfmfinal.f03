! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/F2003/mv_Alloc/ptrAssc/polyPntToDTfmfinal.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : TO is of a poly type which has final subroutine
!*                               FROM is of nonpoly type of child
!*                               Pointer is of a poly type
!*                               defect 322595
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
            final :: finalA
   end type

   type, extends(base) :: A    ! (4,20)
       class(base(k1,:)), allocatable :: x
   end type

   class(base(4,:)), pointer :: p(:)

   contains
       subroutine finalA(a)
            type(base(4,*)), intent(inout) :: A(:)
            print *, "deallocting"
       end subroutine
end module

use m

    type(A(4,:)), target, allocatable :: from(:)
    class(base(4,:)), target, allocatable :: to (:)

    allocate(from(3:5), source = (/ A(4,20)(base(4,20)()),A(4,20)(base(4,20)()),A(4,20)(base(4,20)()) /) )
   ! allocate(from(3:5), source = (/ (A(base()), i=1,3) /) )

    p => from

    call move_alloc(from, to)

    if ( allocated(from) ) error stop 11
    if ( .not. allocated(to)) error stop 13

    if ( lbound(to,1) /= 3 ) error stop  21
    if ( ubound(to,1) /= 5 ) error stop  22
    if ( .not. associated(p, to) ) error stop 23

    deallocate(to)

end
