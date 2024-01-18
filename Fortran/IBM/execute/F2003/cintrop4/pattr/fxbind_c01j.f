! *********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: EXEC_REP=1; $TR_SRC/fxbindc.sh fxbind_c01j bind_c01j
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
!* TEST CASE TITLE              : fxbind_c01j.f
!* TEST CASE TITLE              : BIND(C) attribute
!*
!* PROGRAMMER                   : Yubin Liao
!* DATE                         : Jan. 1, 2004
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab !*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf90
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test: BINC(C) attribute 
!*                                with different intrinsic data type,
!*                                integer*1, integer*2, integer*4,
!*                                integer*8, real*4, real*8, 
!*                                character(1). Using external
!*                                subroutine,interface.C calls Fortran.
!*                                with binding lables
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/05/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890


       subroutine extsub_int(i1, i2, i4, i8) BIND(C, name = "sub_int")
           integer*1 i1
           integer*2 i2
           integer*4 i4
           integer*8 i8
           i1 = i1 + 3
           i2 = i2 + 3
           i4 = i4 + 3
           i8 = i8 + 3
       end subroutine extsub_int

       subroutine extsub_real(r4, r8) BIND(C, name = "sub_real")
           real*4   r4
           real*8   r8
           r4 = r4 * 2
           r8 = r8 * 2
       end subroutine extsub_real

       subroutine extsub_log(l1) BIND(C, name = "sub_log")
           logical*1 l1
           l1 = .true.
       end subroutine extsub_log

       subroutine extsub_comp(co8, co16) BIND(C, name = "sub_comp")
           complex*8   co8
           complex*16  co16
           co8 = (1.0, 1.0)
           co16 = (1.0D0, 1.0D0)
       end subroutine extsub_comp

       subroutine extsub_char(ch1) BIND(C, name = "sub_char")
           character*1 ch1
           ch1 = 'd'
       end subroutine extsub_char

      
