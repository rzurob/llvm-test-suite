! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qreuse=base /tstdev/F2003/mv_Alloc/ptrAssc/polyPntFmUlmtTofinal.f
! opt variations: -ql -qdefaultpv -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM is poly type of child
!*                               Pointer is poly type of base with final sub
!*                               TO is unlimited poly
!*                               defect 322595
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id
        contains
            final :: finalA
   end type

   contains
       subroutine finalA(a)
            type( base(4)), intent(inout) :: a(:)
       end subroutine
end module

use m

    class(*), target,  allocatable :: to(:)
    class(base(4)), pointer :: p(:)

    type, extends(base) :: A    ! (4)
       class(base(k1)), allocatable :: x
    end type

    class(A(4)), target, allocatable :: from(:)

    allocate(from(3), source = (/ (A(4)( x=base(4)(i+10), id=i), i=1,3) /) )

    p => from

    call move_alloc(from, to)

    if ( allocated(from) ) stop 11
    if ( .not. allocated(to)) stop 13

    select type (to)
        type is (A(4))
            if ( .not. associated(p, to) ) stop 23

 	    do i = 1, 3
                if ( .not. allocated(to(i)%x) )  call zzrc(i)
            end do
            if ( to(1)%x%id /= 11 ) stop 31
            if ( to(2)%x%id /= 12 ) stop 33
            if ( to(3)%x%id /= 13 ) stop 35
        class default
            stop 41
    end select
end
