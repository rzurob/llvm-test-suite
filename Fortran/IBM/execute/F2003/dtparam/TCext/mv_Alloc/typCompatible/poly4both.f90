! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/mv_Alloc/typCompatible/poly4both.f
! opt variations: -ql -qreuse=none

! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : poly4both.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/13/2006
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
!*  DESCRIPTION                : FROM/TO are of polymorphic DT
!*                               TO is extended from FROM, direct child of FROM 
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
      type  :: base(k1)    ! (4)
          integer, kind             :: k1
          integer(k1) , allocatable :: i1 
      end type 

      type, extends(base) :: child    ! (4)
          integer(k1) , allocatable:: i2
      end type

      class(child(4)), allocatable :: b

      interface 
           subroutine sub(arg,brg)
               import base
               import child
               
               class(base(4)), allocatable ::  arg
               class(child(4)), allocatable :: brg
           end subroutine
      end interface
end module

use m

    class(base(4)), allocatable :: a

    allocate(b, source = child(4)(10, 20))
    allocate(a, source = base(4)(66))

    call sub(a, b)
    
    if ( allocated(b) ) stop 11

    select type (a)
        type is (child(4))
            if ( allocated(a%i1) ) stop 21
            if ( .not. allocated(a%i2) ) stop 21
            if ( a%i2 /= 10 ) stop 23
        class default
            stop 31
    end select

    end

           subroutine sub(arg,brg)
               use m, only : base, child
               class(base(4)), allocatable :: arg
               class(child(4)), allocatable :: brg

               call move_alloc(brg, arg)

               select type (arg)
                   type is (child(4))
                        call move_alloc(arg%i1, arg%i2)
                   class default
                        stop 31
               end select

           end subroutine
