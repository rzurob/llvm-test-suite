! *********************************************************************
!**********************************************************************
!* ===================================================================
!*
!* DATE                         : Jan. 1, 2004
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test bind(c) module subroutine entry
!*                                with c function pointer as argument.
!*                                Subwoutine is implemented in Fortran
!*                                called from C. Subroutine pass back
!*                                function pointer to C.
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/05/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
contains
     subroutine ext_fsub(f)
       use iso_c_binding
       type(C_FUNPTR) :: f
       return
     entry fsub(f) bind(c)

       call csub(f)

     end subroutine ext_fsub
end module
