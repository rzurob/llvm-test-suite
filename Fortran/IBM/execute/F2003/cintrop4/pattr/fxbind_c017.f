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
!*                                logical(1), character(1). Using external
!*                                subroutine,interface.C calls Fortran.
!*                                with binding labels
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/05/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890


       subroutine sub_int(i1, i2) bind(c, name = "extsub_int")
           use iso_c_binding
           type(C_PTR) :: i1
           type(C_PTR) :: i2
           i1 = i2
       end subroutine sub_int

       subroutine sub_real(r4) bind(c, name = "extsub_real")
           use iso_c_binding
           logical precision_r4
           type(c_ptr) :: r4
           real(c_float), pointer :: fp
           call c_f_pointer(r4, fp)
           if ( .not. precision_r4(fp, 1.0) ) then
             error stop 20
           end if
       end subroutine sub_real

       subroutine sub_log(l1, l2) bind(c, name = "extsub_log")
           use iso_c_binding
           type(C_PTR) ::  l1
           type(c_ptr) ::  l2
           l1 = l2
       end subroutine sub_log

       subroutine sub_char(ch1, ch2) bind(c, name = "extsub_char")
           use iso_c_binding
           type(C_PTR) ::  ch1
           type(c_ptr) ::  ch2
           ch1 = ch2
       end subroutine sub_char

