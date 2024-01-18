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
!*                              logical as argument and void as return. In
!*                              Fortran, associating p to procedure pointer.
!*                              Referencing procedure pointer entity and
!*                              check the correctness of the return value. 
!* ===================================================================

module fptr14
   use ISO_C_BINDING
   interface
       subroutine csub(cptr) bind(c)
          import C_FUNPTR
          type(C_FUNPTR) :: cptr
       end subroutine csub
   end interface
end module fptr14

program procptrBindcFunptr14

   use ISO_C_BINDING
  
   use fptr14

   interface
       subroutine swap(i, j) bind(c)
          import C_BOOL
          logical(C_BOOL) :: i, j
       end subroutine 
   end interface

   type(C_FUNPTR) :: p   
   logical(C_BOOL) :: i, j

   procedure(csub), pointer :: subind =>null()
   procedure(swap), pointer :: funind =>null()

   p = C_NULL_FUNPTR

   i = .true. 
   j = .false. 

   subind => csub   

   if(C_ASSOCIATED(C_FUNLOC(swap), p)) error stop 1_4
   call subind(p)
   if(.not. C_ASSOCIATED(C_FUNLOC(swap), p)) error stop 2_4

   if(ASSOCIATED(funind)) error stop 3_4
   call C_F_PROCPOINTER(p, funind) 
   if(.not. ASSOCIATED(funind)) error stop 4_4

   if( i .neqv. .true.) error stop 5_4
   if( j .neqv. .false.) error stop 6_4

   call funind(i, j)

   if(i .neqv. .false.) error stop 7_4
   if(j .neqv. .true.) error stop 8_4

end program procptrBindcFunptr14
