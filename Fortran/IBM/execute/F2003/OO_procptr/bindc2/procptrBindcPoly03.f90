!*  ===================================================================
!*
!*  DATE                       : 3/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointer with BindC
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*                               interoperable procedure pointer entity as selector.
!* ===================================================================

program procptrBindcPoly03

   use ISO_C_BINDING

   interface
       subroutine csub(cptr) bind(c)
          import C_FUNPTR
          type(C_FUNPTR) :: cptr
       end subroutine csub
   end interface

   interface
       integer(C_INT) function cfun(i) bind(c)
          import C_INT
          integer(C_INT), value :: i
       end function
   end interface

   type(C_FUNPTR) :: p
   integer(C_INT) :: i, j

   procedure(csub), pointer :: subind
   procedure(cfun), pointer :: funind

   nullify(funind)

   p = C_NULL_FUNPTR

   i = 11_C_INT
   j = 22_C_INT

   subind => csub

   if(C_ASSOCIATED(C_FUNLOC(cfun), p)) error stop 1_4
   call subind(p)
   if(.not. C_ASSOCIATED(C_FUNLOC(cfun), p)) error stop 2_4

   if(ASSOCIATED(funind)) error stop 3_4
   call C_F_PROCPOINTER(p, funind)
   if(.not. ASSOCIATED(funind)) error stop 4_4

   associate(As => funind(j))

      if(As .ne. 22) error stop 6_4

   end associate

end program procptrBindcPoly03

