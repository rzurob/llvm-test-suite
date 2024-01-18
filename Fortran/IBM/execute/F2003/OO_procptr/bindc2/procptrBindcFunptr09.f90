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
!*                              pointer, get address of C function. In
!*                              Fortran, associating p to procedure pointer.
!*                              Referencing procedure pointer entity and
!*                              check the correctness of the return value. 
!* ===================================================================

module fptr09
   use ISO_C_BINDING
   interface
       subroutine csub(cptr) bind(c)
          import C_FUNPTR
          type(C_FUNPTR) :: cptr
       end subroutine csub
   end interface
end module fptr09

program procptrBindcFunptr09

   use ISO_C_BINDING
  
   use fptr09

   interface
       subroutine swap(i, j) bind(c)
          import C_INT
          integer(C_INT) :: i, j
       end subroutine 
   end interface

   type(C_FUNPTR) :: p   
   integer(C_INT) :: i, j

   procedure(csub), pointer :: subind =>null()
   procedure(swap), pointer :: funind =>null()

   p = C_NULL_FUNPTR

   i = 10_C_INT
   j = 20_C_INT

   subind => csub   

   if(C_ASSOCIATED(C_FUNLOC(swap), p)) error stop 1_4
   call subind(p)
   if(.not. C_ASSOCIATED(C_FUNLOC(swap), p)) error stop 2_4

   if(ASSOCIATED(funind)) error stop 3_4
   call C_F_PROCPOINTER(p, funind) 
   if(.not. ASSOCIATED(funind)) error stop 4_4

   if( i .ne. 10_C_INT) error stop 5_4
   if( j .ne. 20_C_INT) error stop 6_4

   call funind(i, j)

   if(i .ne. 20_C_INT) error stop 7_4
   if(j .ne. 10_C_INT) error stop 8_4

end program procptrBindcFunptr09
