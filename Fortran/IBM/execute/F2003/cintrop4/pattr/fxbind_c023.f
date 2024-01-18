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
!* DESCRIPTION                  : Test: BINC(C) attribute
!*                                with C_ptr type of integer, real
!*                                logical(1), character(1). Using module
!*                                entry. C calls Fortran.
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
       subroutine sextsub_int(i1, i2)
           use iso_c_binding
           type(C_PTR) :: i1
           type(C_PTR) :: i2
           return
       entry extsub_int(i1, i2) bind(c)
           i1 = i2
       end subroutine sextsub_int

       subroutine sextsub_real(r4)
           use iso_c_binding
           logical precision_r4
           type(c_ptr) :: r4
           real(c_float), pointer :: fp
           return
       entry extsub_real(r4) bind(c)
           call c_f_pointer(r4, fp)
           if ( .not. precision_r4(fp, 1.0) ) then
             error stop 20
           end if
       end subroutine sextsub_real

       subroutine sextsub_log(l1, l2)
           use iso_c_binding
           type(C_PTR) ::  l1
           type(c_ptr) ::  l2
           return
       entry extsub_log(l1, l2) bind(c)
           l1 = l2
       end subroutine sextsub_log

       subroutine sextsub_char(ch1, ch2)
           use iso_c_binding
           type(C_PTR) ::  ch1
           type(c_ptr) ::  ch2
           return
       entry extsub_char(ch1, ch2) bind(c)
           ch1 = ch2
       end subroutine sextsub_char
end module m
