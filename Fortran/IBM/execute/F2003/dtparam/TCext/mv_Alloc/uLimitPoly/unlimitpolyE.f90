! GB DTP extension using:
! ftcx_dtp -ql -qreuse=base /tstdev/F2003/mv_Alloc/uLimitPoly/unlimitpolyE.f
! opt variations: -qnol -qreuse=none

! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : unlimitpolyE.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/01/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*                              
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : FROM and TO are unlimited polymorphic,
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
           procedure, nopass :: swap => setD
    end type

    type, extends(A2D) :: A3D    ! (20,4)
        integer(k1) z
    end type

    contains
        subroutine setD (q, r)
           class(*), allocatable, intent(inout) :: q, r
           class(*), allocatable :: local 

           call move_alloc(q, local)
           call move_alloc(r, q) 
           call move_alloc(local, r)

        end subroutine 

end module

program main
use m

    type(A2D(20,4)) :: a1
    class (*), allocatable :: a2, a3

    allocate(a2, source =  A2D(20,4)(40,50) )
    allocate(a3, source =  A2D(20,4)(80,90) )

    call a1%swap(a2,a3 )

    if ( .not. allocated(a2) ) stop 9
    if ( .not. allocated(a3) ) stop 10

    select type (a2)
        type is (A2D(*,4))
            print *, "result A2D", a2%x, a2%y
        type is (A3D(*,4))
            print *, "result A2D", a2%x, a2%y, a2%z
	class default
	    stop 51
    end select 

    select type (a3)
        type is (A2D(*,4))
            print *, "result A2D", a3%x, a3%y
        type is (A3D(*,4))
            print *, "result A2D", a3%x, a3%y, a3%z
	class default
	    stop 52
    end select 

end
