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
!*                              pass null function pointer p to C function
!*                              pointer, get address of C function with
!*                              char as argument and void as return. In
!*                              Fortran, associating p to procedure pointer.
!*                              Referencing procedure pointer entity and
!*                              check the correctness of the return value. 
!* ===================================================================

module fptr13
   use ISO_C_BINDING
   interface
       subroutine csub(cptr) bind(c)
          import C_FUNPTR
          type(C_FUNPTR) :: cptr
       end subroutine csub
   end interface
end module fptr13

program procptrBindcFunptr13

   use ISO_C_BINDING
  
   use fptr13

   interface
       subroutine swap(i, j) bind(c)
          import C_CHAR
          character(C_CHAR) :: i, j
       end subroutine 
   end interface

   type(C_FUNPTR) :: p   
   character(C_CHAR) :: i, j

   procedure(csub), pointer :: subind =>null()
   procedure(swap), pointer :: funind =>null()

   p = C_NULL_FUNPTR

   i = "A" 
   j = "Z" 

   subind => csub   

   if(C_ASSOCIATED(C_FUNLOC(swap), p)) error stop 1_4
   call subind(p)
   if(.not. C_ASSOCIATED(C_FUNLOC(swap), p)) error stop 2_4

   if(ASSOCIATED(funind)) error stop 3_4
   call C_F_PROCPOINTER(p, funind) 
   if(.not. ASSOCIATED(funind)) error stop 4_4

   if( i .ne. "A") error stop 5_4
   if( j .ne. "Z") error stop 6_4

   call funind(i, j)

   if(i .ne. "Z") error stop 7_4
   if(j .ne. "A") error stop 8_4

end program procptrBindcFunptr13
