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
!*                              function pointer which points to C function with int
!*                              argument and void return, as actual argument 
!*                              passed directly to C  through procedure
!*                              pointer reference. 
!* ===================================================================

module fptr05
   use ISO_C_BINDING
   interface
       subroutine csub(i, cptr) bind(c)
          import C_INT, C_FUNPTR
          integer(C_INT) :: i
          type(C_FUNPTR) :: cptr
       end subroutine csub
   end interface
end module fptr05

program procptrBindcFunptr05

   use ISO_C_BINDING
  
   use fptr05

   interface
       subroutine swap(i, j) bind(c)
          import C_INT
          integer(C_INT) :: i, j
       end subroutine 
   end interface

   integer(C_INT) :: i 

   procedure(csub), pointer :: subind 

   subind => csub   

   i = 34_C_INT

   call subind(i, C_FUNLOC(swap))

   if(i .ne. 22) error stop 1_4 

end program procptrBindcFunptr05
