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
!*                              function pointer which points to C function with char 
!*                              argument and an char return, as actual argument 
!*                              passed to interoperable subprogram through procedure
!*                              pointer reference. Use C_F_PROCPOINTER to
!*                              associate procedure pointer with c address of
!*                              procedure. 
!* ===================================================================

module fptr07
   use ISO_C_BINDING
   interface
       subroutine csub(i, cptr) bind(c)
          import C_CHAR, C_FUNPTR
          character(C_CHAR) :: i
          type(C_FUNPTR) :: cptr
       end subroutine csub
   end interface
end module fptr07

program procptrBindcFunptr07

   use ISO_C_BINDING
  
   use fptr07

   interface
       character(C_CHAR) function cfun(i) bind(c)
          import C_CHAR
          character(C_CHAR), value :: i
       end function 
   end interface

   character(C_CHAR) :: i 

   procedure(csub), pointer :: subind =>null()

   if(ASSOCIATED(subind)) error stop 1_4
   call C_F_PROCPOINTER(C_FUNLOC(csub),subind)
   if(.not. ASSOCIATED(subind)) error stop 2_4

   i = 'B'

   call subind(i, C_FUNLOC(cfun))

   if(i .ne. 'A') error stop 3_4 

end program procptrBindcFunptr07
