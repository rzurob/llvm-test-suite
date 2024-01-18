!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 3/01/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointer with BindC 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                :  
!*                              pass null function pointer to C function
!*                              pointer. In Fortran , check whether function
!*                              pointer associated with C address.  
!* ===================================================================

module fptr04
   use ISO_C_BINDING
   interface
       subroutine csub(i, cptr) bind(c)
          import C_INT, C_FUNPTR
          integer(C_INT) :: i
          type(C_FUNPTR) :: cptr
       end subroutine csub
   end interface
end module fptr04

program procptrBindcFunptr04

   use ISO_C_BINDING
  
   use fptr04

   interface
       integer(C_INT) function cfun(i) bind(c)
          import C_INT
          integer(C_INT),value :: i
       end function
   end interface

   integer(C_INT) :: i 
   type(C_FUNPTR) :: p   

   procedure(csub), pointer :: subind 

   p = C_NULL_FUNPTR

   subind => csub   

   i = 34_C_INT

   if(C_ASSOCIATED(C_FUNLOC(cfun), p)) error stop 1_4
   call subind(i, p)
   if(.not. C_ASSOCIATED(C_FUNLOC(cfun), p)) error stop 2_4

   if(i .ne. 22) error stop 3_4 

end program procptrBindcFunptr04
