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
!*                                C_F_POINTER 
!*  (316905)
!* ===================================================================

program procptrBindMproc04d

   use ISO_C_BINDING,ONLY : C_F_POINTER, C_FUNPTR, C_PTR

   type(C_PTR) :: cptr

   integer, pointer :: ptr

   call sub(cptr, ptr)

   contains

      subroutine sub(arg1, arg2)
         type(C_PTR), intent(out) :: arg1
         integer, pointer, intent(in) :: arg2

         call C_F_POINTER(arg1, arg2)

      end subroutine

end program procptrBindMproc04d

