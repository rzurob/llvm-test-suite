!*  ===================================================================
!*
!*  DATE                       : 3/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointer with BindC
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :  TC for C_ASSOCIATED
!*
!* ===================================================================

program procptrBindMproc01

   use ISO_C_BINDING

   type(C_FUNPTR) :: cfuncptr2
   type(C_PTR) :: cptr1

   integer :: int

   interface
       subroutine csub(i) bind(c)
          integer i
       end subroutine csub
   end interface

   procedure(csub), pointer :: pbind => null()

   cfuncptr2 = C_NULL_FUNPTR
   cptr1 = C_NULL_PTR

   if(C_ASSOCIATED(C_NULL_PTR) .neqv. .false.) error stop 1_4
   if(C_ASSOCIATED(C_NULL_FUNPTR) .neqv. .false.) error stop 2_4
   if(C_ASSOCIATED(C_NULL_PTR, cptr1 ) .neqv. .false.) error stop 3_4
   if(C_ASSOCIATED(cfuncptr2, C_NULL_FUNPTR) .neqv. .false.) error stop 4_4
   if(C_ASSOCIATED(C_FUNLOC(pbind)) .eqv. .true.) error stop 5_4

   pbind=>csub

   if(C_ASSOCIATED(C_FUNLOC(pbind)) .neqv. .true.) error stop 6_4

end program procptrBindMproc01

    subroutine csub(i)
       integer i
    end subroutine csub

