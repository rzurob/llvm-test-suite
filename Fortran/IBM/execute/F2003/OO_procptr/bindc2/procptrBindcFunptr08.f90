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
!*                              pass null function pointer(structure
!*                              component) to C function
!*                              pointer, get address of C function. In
!*                              Fortran, associating p to procedure pointer.
!*                              Referencing procedure pointer entity and
!*                              check the correctness of the return value. 
!* ===================================================================

module fptr08
   use ISO_C_BINDING
   interface
       subroutine csub(cptr) bind(c)
          import C_FUNPTR
          type(C_FUNPTR) :: cptr
       end subroutine csub
   end interface
end module fptr08

program procptrBindcFunptr08

   use ISO_C_BINDING
  
   use fptr08

   interface
       integer(C_INT) function cfun(i) bind(c)
          import C_INT
          integer(C_INT), value :: i
       end function
   end interface

   type(C_FUNPTR) :: p   
   integer(C_INT) :: i, j

   procedure(csub), pointer :: subind =>null()
   procedure(cfun), pointer :: funind =>null()

   p = C_NULL_FUNPTR

   i = 11_C_INT
   j = 22_C_INT

   subind => csub   

   if(C_ASSOCIATED(C_FUNLOC(cfun), p)) error stop 1_4
   call subind(p)
   if(.not. C_ASSOCIATED(C_FUNLOC(cfun), p)) error stop 2_4

   if(ASSOCIATED(funind)) error stop 3_4
   call C_F_PROCPOINTER(p, funind) 
   if(.not. ASSOCIATED(funind)) error stop 4_4

   if( i .ne. 11_C_INT) error stop 5_4

   i = funind(j)

   if(i .ne. 22_C_INT) error stop 6_4

end program procptrBindcFunptr08
