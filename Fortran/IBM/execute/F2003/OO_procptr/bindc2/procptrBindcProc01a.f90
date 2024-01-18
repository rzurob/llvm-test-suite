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
!*  DESCRIPTION                :  simple test on void * as the C return.
!*                                associate procedure pointer with c function
!*                                pointer pointing to function returning
!*                                void *.  
!* ===================================================================

program procptrBindcProc01a 

   use ISO_C_BINDING

   interface
       type(C_PTR) function match(arg1, arg2) bind(c)
          import C_PTR, C_CHAR
          character(C_CHAR) :: arg2(10)
          character(C_CHAR),value :: arg1
       end function
   end interface

   character(C_CHAR) :: ch
   character(C_CHAR), pointer :: p
   character(C_CHAR), target :: s(10), cch

   type(C_PTR) :: res

   procedure(match), pointer :: fun_match => null()

   s = 'A'

   s(7) = 'Z'

   ch = 'Z'
   cch = 'G'

   p => cch 

   if(ASSOCIATED(fun_match)) error stop 1_4
   call C_F_PROCPOINTER(C_FUNLOC(match), fun_match)
   if(.not. ASSOCIATED(fun_match)) error stop 2_4

   res = fun_match(ch, s)

   if(p /= 'G') error stop 3_4
   call C_F_POINTER(res,p)
   if(p /= 'Z') error stop 4_4

end program procptrBindcProc01a 

