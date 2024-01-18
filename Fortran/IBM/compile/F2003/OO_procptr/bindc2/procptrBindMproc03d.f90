!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 3/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointer with BindC 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                :  diagnostic TC on INTENT for 
!*                                C_F_PROCPOINTER 
!* ===================================================================

program procptrBindMproc03d

   use ISO_C_BINDING,ONLY : C_F_PROCPOINTER, C_FUNPTR, C_PTR

   type(C_FUNPTR) :: cfptr

   interface
       subroutine csub
       end subroutine csub
   end interface

   procedure(csub), pointer :: pfptr

   call sub(cfptr, pfptr)

   contains
 
      subroutine sub(arg1, arg2)
         type(C_FUNPTR), intent(out) :: arg1  
         procedure(csub), intent(in), pointer :: arg2
 
         call C_F_PROCPOINTER(arg1, arg2) 

      end subroutine

end program procptrBindMproc03d

