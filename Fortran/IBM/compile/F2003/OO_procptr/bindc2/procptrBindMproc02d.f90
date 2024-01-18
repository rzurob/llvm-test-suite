!*  ===================================================================
!*
!*  DATE                       : 3/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointer with BindC
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :  diagnostic TC for C_ASSOCIATE()
!*
!* ===================================================================

program procptrBindMproc02d

   use ISO_C_BINDING,ONLY : C_F_PROCPOINTER, C_FUNPTR, C_PTR, C_ASSOCIATED

   type(C_FUNPTR) :: cfuncptr1(5)
   type(C_FUNPTR) :: cfuncptr2
   type(C_PTR) :: cptr1
   type(C_PTR) :: cptr2(2)

   integer, pointer :: ip
   integer :: int

   interface
       subroutine csub() bind(c)
       end subroutine csub
   end interface

   procedure(csub), pointer :: pnobind

   print *, C_ASSOCIATED(cfuncptr2, cptr1)
   print *, C_ASSOCIATED(cptr1, cfuncptr2)
   print *, C_ASSOCIATED(pnobind)
   print *, C_ASSOCIATED(cfuncptr2, pnobind)
   print *, C_ASSOCIATED(ip,ip)
   print *, C_ASSOCIATED(cfuncptr1)
   print *, C_ASSOCIATED(cptr2)
   print *, C_ASSOCIATED(cfuncptr1, cfuncptr2)
   print *, C_ASSOCIATED(cptr1, cptr2)

end program procptrBindMproc02d

