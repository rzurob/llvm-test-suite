!*  ===================================================================
!*
!*  DATE                       : 3/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointer with BindC
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :  dummy function pointer with VALUE attribute.
!*                                function pointer points function taking void *
!*                                as its argument and return int.
!* ===================================================================

program procptrBindcFunptr18

   use ISO_C_BINDING

   interface
      integer(C_INT) function greeting(arg1, arg2) bind(c)
        import C_FUNPTR, C_PTR, C_INT
        type(C_FUNPTR), VALUE :: arg1
        type(C_PTR), VALUE :: arg2
      end function
   end interface

   interface
      integer(C_INT) function use_int(arg) bind(c)
        import C_PTR, C_INT
        type(C_PTR), VALUE :: arg
      end function
   end interface

   integer(C_INT), target :: i_age
   type(C_PTR) :: p

   procedure(greeting), pointer:: pt

   pt => greeting

   i_age = 22_C_INT

   p = C_LOC(i_age)

   if(pt(C_FUNLOC(use_int), p) .ne. 34) error stop 1_4

end program procptrBindcFunptr18
