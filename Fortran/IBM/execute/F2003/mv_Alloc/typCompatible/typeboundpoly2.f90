! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : typeboundpoly2.f 
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

    type A2D
        integer x,y
        contains
           procedure :: swap => setD
    end type

    type, extends(A2D) :: A3D
        integer z
    end type

    contains
        subroutine setD ( p, q, r)
           class(A2D), intent(in) :: p
           class(A2D), allocatable, intent(inout) :: q, r
           class(A2D), allocatable :: local 

           call move_alloc(q, local)
           call move_alloc(r, q) 
           call move_alloc(local, r)

        end subroutine 

end module

program main
use m

    class (A2D), allocatable :: a1
    class (A2D), allocatable :: a2, a3

    allocate(a1, source =  A2D(10,20 ))
    allocate(a2, source =  A3D(40,50,60) )
    allocate(a3, source =  A3D(70,80,90) )

    call a1%swap(a2,a3 )

    select type (a2)
        type is (A3D)
            select type (a3)
                type is (A3D)
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
