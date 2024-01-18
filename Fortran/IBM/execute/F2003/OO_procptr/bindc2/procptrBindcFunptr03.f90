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
!*                              argument and an int return, as actual argument
!*                              passed to interoperable subprogram through procedure
!*                              pointer reference. 
!*                              function pointer is array.
!* ===================================================================

module fptr03
   use ISO_C_BINDING
   interface
       subroutine csub(x, y, z, cptr) bind(c)
          import C_INT, C_FUNPTR
          integer(C_INT) :: x, y, z 
          type(C_FUNPTR) :: cptr(3)
       end subroutine csub
   end interface
end module fptr03

program procptrBindcFunptr03

   use ISO_C_BINDING
  
   use fptr03

   interface
       integer(C_INT) function cfun1(i) bind(c)
          import C_INT
          integer(C_INT),value :: i
       end function 
   end interface

   interface
       integer(C_INT) function cfun2(i) bind(c)
          import C_INT
          integer(C_INT),value :: i
       end function
   end interface

   interface
       integer(C_INT) function cfun3(i) bind(c)
          import C_INT
          integer(C_INT),value :: i
       end function
   end interface

   procedure(csub), pointer :: subind

   type(C_FUNPTR) :: fp(3)
   integer(C_INT) :: i , j, k

   fp(1) = C_FUNLOC(cfun1)
   if(.not. C_ASSOCIATED(C_FUNLOC(cfun1), fp(1))) error stop 1_4   
   fp(2) = C_FUNLOC(cfun2)
   if(.not. C_ASSOCIATED(C_FUNLOC(cfun2), fp(2))) error stop 2_4
   fp(3) = C_FUNLOC(cfun3)
   if(.not. C_ASSOCIATED(C_FUNLOC(cfun3), fp(3))) error stop 3_4

   subind => csub   

   i = 1_C_INT
   j = 2_C_INT
   k = 3_C_INT

   call subind(i, j, k, fp)

   if(i .ne. 11_C_INT) error stop 4_4 
   if(j .ne. 12_C_INT) error stop 5_4
   if(k .ne. 13_C_INT) error stop 6_4

end program procptrBindcFunptr03
