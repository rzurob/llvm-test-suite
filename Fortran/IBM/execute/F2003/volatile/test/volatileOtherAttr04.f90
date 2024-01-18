!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 12/06/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : VOLATILE with allocatable 
!*                                
!*
!*  DESCRIPTION                :  
!*            functional test VOLATILE compatible with allocatable.
!* ===================================================================

  program volatileOtherAttr04

    class(*), VOLATILE, allocatable :: x(:)

    allocate (x(3), source=(/10,20,30/))

    select type (y => cshift (x, 1))
        type is (integer)
        class default
            error stop 1_4
    end select

  end program volatileOtherAttr04
