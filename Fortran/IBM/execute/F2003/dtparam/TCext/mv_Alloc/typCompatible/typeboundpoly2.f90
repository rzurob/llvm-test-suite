! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=none /tstdev/F2003/mv_Alloc/typCompatible/typeboundpoly2.f
! opt variations: -qnol -qnodeferredlp -qreuse=base

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM and TO are polymorphic,
!*                               MOVE_ALLOC are called in a type-bound proc
!*                               FROM&TO has intent(inout) attribute
!*                               FROM&TO are dummy args of type_bound proc
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

    type A2D(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      x,y
        contains
           procedure :: swap => setD
    end type

    type, extends(A2D) :: A3D(n2,k2)    ! (20,4,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      z
    end type

    contains
        subroutine setD ( p, q, r)
           class(A2D(*,4)), intent(in) :: p
           class(A2D(:,4)), allocatable, intent(inout) :: q, r
           class(A2D(:,4)), allocatable :: local

           call move_alloc(q, local)
           call move_alloc(r, q)
           call move_alloc(local, r)

        end subroutine

end module

program main
use m

    class (A2D(:,4)), allocatable :: a1
    class (A2D(:,4)), allocatable :: a2, a3

    allocate(a1, source =  A2D(20,4)(10,20 ))
    allocate(a2, source =  A3D(20,4,20,4)(40,50,60) )
    allocate(a3, source =  A3D(20,4,20,4)(70,80,90) )

    call a1%swap(a2,a3 )

    select type (a2)
        type is (A3D(*,4,*,4))
            select type (a3)
                type is (A3D(*,4,*,4))
                    print *, "result A3D"
                    print *, a2%x, a2%y, a2%z
                    print *, a3%x, a3%y, a3%z
                class default
                   stop 31
           end select
        class default
             stop 41
    end select

end
