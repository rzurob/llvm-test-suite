! *********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: EXEC_REP=1; $TR_SRC/fxbindc.sh fxbind_c019 bind_c011
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : fxbind_c019.f
!* TEST CASE TITLE              : BIND(C) attribute
!*
!* PROGRAMMER                   : Yubin Liao
!* DATE                         : Jan. 1, 2004
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf90
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test: BINC(C) attribute 
!*                                with C_ptr type of integer, real
!*                                logical(1), character(1). Using external
!*                                entry. C calls Fortran. with binding
!*                                labels.
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/05/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890


       subroutine sextsub_int(i1, i2) 
           use iso_c_binding 
           type(C_PTR) :: i1
           type(C_PTR) :: i2
           return
       entry bextsub_int(i1, i2) bind(c, name = "extsub_int")
           i1 = i2
       end subroutine sextsub_int
      
       subroutine sextsub_real(r4) 
           use iso_c_binding
           logical precision_r4
           type(c_ptr) :: r4
           real(c_float), pointer :: fp
           return
       entry bextsub_real(r4) bind(c, name = "extsub_real")
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
       entry bextsub_log(l1, l2) bind(c, name = "extsub_log")
           l1 = l2
       end subroutine sextsub_log

       subroutine sextsub_char(ch1, ch2)  
           use iso_c_binding
           type(C_PTR) ::  ch1
           type(c_ptr) ::  ch2
           return
       entry bextsub_char(ch1, ch2) bind(c, name = "extsub_char")
           ch1 = ch2
       end subroutine sextsub_char

