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
!*                              function pointer is structure component
!* ===================================================================

module fptr02
   use ISO_C_BINDING

   type,bind(c) :: dt
       type(C_FUNPTR) :: fp
       integer(C_INT) :: ig
   end type 

   interface
       subroutine csub(i, cptr) bind(c)
          import C_INT, C_FUNPTR, dt
          integer(C_INT) :: i
          type(dt) :: cptr
       end subroutine csub
   end interface
end module fptr02

program procptrBindcFunptr02

   use ISO_C_BINDING
  
   use fptr02

   interface
       integer(C_INT) function cfun(i) bind(c)
          import C_INT
          integer(C_INT),value :: i
       end function 
   end interface

   integer(C_INT) :: i 

   type(dt) :: dt_obj

   procedure(csub), pointer :: subind 

   subind => csub   

   i = 20_C_INT

   dt_obj%ig = 34_C_INT
   dt_obj%fp = C_FUNLOC(cfun)

   if(.not. C_ASSOCIATED(dt_obj%fp)) error stop 1_4
   if(.not. C_ASSOCIATED(dt_obj%fp, C_FUNLOC(cfun))) error stop 2_4

   if(i .ne. 20) error stop 3_4

   call subind(i, dt_obj)

   if(i .ne. 22) error stop 4_4 

end program procptrBindcFunptr02
