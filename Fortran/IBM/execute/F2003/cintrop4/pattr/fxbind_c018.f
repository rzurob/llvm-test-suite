! *********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: EXEC_REP=1; $TR_SRC/fxbindc.sh fxbind_c018 bind_c010
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
!* TEST CASE TITLE              : fxbind_c018.f
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
!*                                with different intrinsic data type,
!*                                integer*1, logical*1
!*                                character(1). Using external
!*                                entry. C calls Fortran.
!*                                The arguments are passed by typeless
!*                                constants. with binding labels
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/05/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890


       subroutine sextsub_int(i1, i2)  
           integer*1 i1
           integer*8 i2
           return
       entry bextsub_int(i1, i2) bind(c, name = "extsub_int")
           i1 = b'00001111'
           i2 = x'000000000000000F' 
       end subroutine sextsub_int


       subroutine sextsub_log(l1)  
           logical*1 l1
           return
       entry bextsub_log(l1) bind(c, name = "extsub_log")
           l1 = b'00000001'
       end subroutine sextsub_log

       subroutine sextsub_char(ch1)  
           character*1 ch1
           return
       entry bextsub_char(ch1) bind(c, name = "extsub_char")
           ch1 = x'61'
       end subroutine sextsub_char

