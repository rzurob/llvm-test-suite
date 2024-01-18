!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 3/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointer with BindC 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                :  diagnostic TC for C_F_PROCPOINTER()
!*                                
!* ===================================================================

program procptrBindMproc01d

   use ISO_C_BINDING

   type(C_FUNPTR) :: cfuncptr1(5)
   type(C_FUNPTR) :: cfuncptr2
   type(C_PTR) :: cptr1

   integer, pointer :: ip 
   integer, pointer :: iparray(:)
   integer :: int

   interface
       subroutine csub1() bind(c)
       end subroutine csub1
   end interface

   interface
       subroutine csub2(i) bind(c)
        import C_INT
       end subroutine csub2
   end interface

   procedure(csub1), pointer :: pfuncptr
   procedure(csub2), pointer :: pbind

   cptr1=C_NULL_PTR

   cfuncptr2 = C_FUNLOC(csub1)

    call C_F_PROCPOINTER()
    call C_F_PROCPOINTER(cfuncptr2)
    call C_F_PROCPOINTER(pfuncptr)
    call C_F_PROCPOINTER(cfuncptr1, pfuncptr) 
    call C_F_PROCPOINTER(cptr1, pfuncptr)

    call C_F_PROCPOINTER(int, pfuncptr)
    call C_F_PROCPOINTER(ip, pfuncptr)
    call C_F_PROCPOINTER(cfuncptr2, ip)
    call C_F_PROCPOINTER(cfuncptr2, cfuncptr2)
    call C_F_PROCPOINTER(cfuncptr2, iparray)
    call C_F_PROCPOINTER(C_FUNLOC(pbind), C_FUNLOC(pbind))
    call C_F_PROCPOINTER(C_FUNLOC(pbind), cfuncptr2)
    call C_F_PROCPOINTER(pbind, pbind) 

end program procptrBindMproc01d

