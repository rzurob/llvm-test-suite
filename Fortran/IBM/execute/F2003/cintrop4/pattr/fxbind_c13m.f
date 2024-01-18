! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: EXEC_REP=1; $TR_SRC/fxbindc1.sh fxbind_c13m bind_c13m
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
!* DESCRIPTION                  : Test: subroutine with BINd(C) attribute
!*                                with real16
!*                                Using external
!*                                subroutine,interface.C calls fortran.
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/05/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890



       subroutine extsub_real(r16) bind(c)
           implicit none
           real*16  r16
           r16 = r16 * 2
       end subroutine extsub_real

       subroutine extsub_comp(co32) bind(c)
           implicit none
           complex*32 co32
           co32 = co32 + (1.0Q0, 1.0Q0)
       end subroutine extsub_comp


