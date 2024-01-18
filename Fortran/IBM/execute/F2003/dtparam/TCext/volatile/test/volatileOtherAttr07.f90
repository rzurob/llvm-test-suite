! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/volatile/test/volatileOtherAttr07.f
! opt variations: -qnol

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
       type base(n1,k1)    ! (20,4)
           integer, kind :: k1
           integer, len  :: n1
           integer(k1)   :: x
           contains
           final :: finalizeBase
       end type
   
       type, extends(base) :: child    ! (20,4)
           contains
           final :: finalizeChild
       end type
   
       contains
       subroutine finalizeBase (b1)
           type (base(*,4)), intent(inout) :: b1
           print *, 'finalizeBase'
       end subroutine
   
       subroutine finalizeChild (b2)
           type (child(*,4)), intent(inout) :: b2
           print *, 'finalizeChild'
       end subroutine

   end module
  
   program volatileOtherAttr07
    use m

    call sub()
   end program volatileOtherAttr07

   subroutine sub()
       use m
       type(base(20,4))  :: dt1
       type(child(20,4)), STATIC :: dt2
       VOLATILE :: dt2

   end subroutine
   
