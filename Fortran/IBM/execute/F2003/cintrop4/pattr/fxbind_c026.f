! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: EXEC_REP=1; $TR_SRC/fxbindc.sh fxbind_c026 bind_c010
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
!*
!* DATE                         : Jan. 1, 2004
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test: BINC(C) attribute
!*                                with different intrinsic data type,
!*                                integer*1, logical*1
!*                                character(1). Using module
!*                                subroutine. C calls Fortran.
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

module m
contains
       subroutine sub_int(i1, i2) bind(c, name = "extsub_int")
           integer*1 i1
           integer*8 i2
           i1 = b'00001111'
           i2 = x'000000000000000F'
       end subroutine sub_int


       subroutine sub_log(l1) bind(c, name = "extsub_log")
           logical*1 l1
           l1 = b'00000001'
       end subroutine sub_log

       subroutine sub_char(ch1) bind(c, name = "extsub_char")
           character*1 ch1
           ch1 = x'61'
       end subroutine sub_char
end module m
