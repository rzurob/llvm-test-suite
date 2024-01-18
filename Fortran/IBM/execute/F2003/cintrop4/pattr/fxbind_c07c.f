! *********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: EXEC_REP=1; $TR_SRC/fxbindc.sh fxbind_c07c bind_c07c
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
!* TEST CASE TITLE              : fxbind_c07c.f
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
!*                                integer*1, integer*2, integer*4,
!*                                integer*8, real*4, real*8, real*16,
!*                                byte, character(1). Using external
!*                                subroutine,interface. C calls Fortran.
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/05/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890



   
       subroutine extsub_int(i1, i2, i4, i8, i81) BIND(C)
           integer*1, value :: i1
           integer*2, value :: i2
           integer*4, value :: i4
           integer*8, value :: i8
           integer*8 i81
           i81 = i1 + i2 + i4 + i8
           i1 = i1 + 3
           i2 = i2 + 3
           i4 = i4 + 3
           i8 = i8 + 3
       end subroutine extsub_int

       subroutine extsub_real(r4, r8, r161) BIND(C)
           real*4, value  ::  r4
           real*8, value  ::  r8
           real*8 r161
           r161 = r4 + r8 
           r4 = r4 * 2
           r8 = r8 * 2
           r16 = r16 * 2
       end subroutine extsub_real

       subroutine extsub_log(l1, l11) BIND(C)
           logical*1, value :: l1
           logical*1 l11
           l11 = l1
           l1 = .true.
       end subroutine extsub_log

       subroutine extsub_comp(co8, co16, co81, co161) BIND(C)
           complex*8, value  ::  co8
           complex*16, value ::  co16
           complex*8 co81
           complex*16 co161
           co81 = co8
           co161 = co16
           co8 = (1.0, 1.0)
           co16 = (1.0D0, 1.0D0)
       end subroutine extsub_comp

       subroutine extsub_char(ch1, ch11) BIND(C)
           character*1, value :: ch1
           character*1 ch11
           ch11 = ch1
           ch1 = 'd'
       end subroutine extsub_char

