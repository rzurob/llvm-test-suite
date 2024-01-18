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
!*                               interoperable procedure pointer entity as expression
!*                               in defined assignment 
!* ===================================================================
module mdefassign

 interface assignment(=)
    subroutine char_to_integer(arg1, arg2)
          integer, intent(out) :: arg1
          character*1, intent(in) :: arg2
    end subroutine
 end interface

end module mdefassign

program procptrBindcDefAssign 

   use ISO_C_BINDING

   use  mdefassign

   interface
       subroutine csub(cptr) bind(c)
          import C_FUNPTR
          type(C_FUNPTR) :: cptr
       end subroutine csub
   end interface

   interface
       character(C_CHAR) function cfun(i) bind(c)
          import C_CHAR
          character(C_CHAR), value :: i
       end function
   end interface

   type(C_FUNPTR) :: p
   character(C_CHAR) :: i, j
   integer(C_INT) :: result

   procedure(csub), pointer :: subind
   procedure(cfun), pointer :: funind =>null()

   p = C_NULL_FUNPTR

   j = 'A' 

   subind => csub

   if(C_ASSOCIATED(C_FUNLOC(cfun), p)) error stop 1_4
   call subind(p)
   if(.not. C_ASSOCIATED(C_FUNLOC(cfun), p)) error stop 2_4

   if(ASSOCIATED(funind)) error stop 3_4
   call C_F_PROCPOINTER(p, funind)
   if(.not. ASSOCIATED(funind)) error stop 4_4

   if(funind(j) .ne. 'B') error stop 5_4

   result = funind(j) 

   if(result .ne. 66) error stop 6_4

end program procptrBindcDefAssign 

subroutine char_to_integer(arg1, arg2)
      integer, intent(out) :: arg1
      character*1, intent(in) :: arg2
      arg1 = ichar(arg2)
end subroutine
