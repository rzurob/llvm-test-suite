! *********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: EXEC_REP=1; $TR_SRC/fxbindc.sh fxbind_c022 bind_c010
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
!* TEST CASE TITLE              : fxbind_c022.f
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
!*                                character(1). Using module 
!*                                entry. C calls Fortran.
!*                                The arguments are passed by typeless
!*                                constants.
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
           integer*1 i1
           integer*8 i2
           return
       entry extsub_int(i1, i2) bind(c)
           i1 = b'00001111'
           i2 = x'000000000000000F' 
       end subroutine sextsub_int


       subroutine sextsub_log(l1)  
           logical*1 l1
           return
       entry extsub_log(l1) bind(c)
           l1 = b'00000001'
       end subroutine sextsub_log

       subroutine sextsub_char(ch1)  
           character*1 ch1
           return
       entry extsub_char(ch1) bind(c)
           ch1 = x'61'
       end subroutine sextsub_char
end module m
