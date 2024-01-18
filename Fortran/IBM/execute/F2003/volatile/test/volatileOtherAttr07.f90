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
!*  PRIMARY FUNCTIONS TESTED   : VOLATILE with STATIC attribute 
!*                                
!*
!*  DESCRIPTION                :  
!*            functional test VOLATILE compatible with STATIC attribute 
!* ===================================================================

   module m
       type base
           integer :: x
           contains
           final :: finalizeBase
       end type
   
       type, extends(base) :: child
           contains
           final :: finalizeChild
       end type
   
       contains
       subroutine finalizeBase (b1)
           type (base), intent(inout) :: b1
           print *, 'finalizeBase'
       end subroutine
   
       subroutine finalizeChild (b2)
           type (child), intent(inout) :: b2
           print *, 'finalizeChild'
       end subroutine

   end module
  
   program volatileOtherAttr07
    use m

    call sub()
   end program volatileOtherAttr07

   subroutine sub()
       use m
       type(base)  :: dt1
       type(child), STATIC :: dt2
       VOLATILE :: dt2

   end subroutine
   
