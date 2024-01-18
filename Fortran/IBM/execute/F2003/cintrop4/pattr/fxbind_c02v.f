! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: EXEC_REP=1; $TR_SRC/fxbindc.sh fxbind_c02v bind_c01v
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
!* DESCRIPTION                  : Test: BINC(C) attribute with binding lables.
!*                                with different intrinsic data type,
!*                                integer*1, integer*2, integer*4,
!*                                integer*8, real*4, real*8,
!*                                character(1). Using module
!*                                subroutine, entry. C calls Fortran.
!*                                Both subroutine and entry contain
!*                                bind(c) attribute.
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
       subroutine sextsub_int(si1, si2, si4, si8) bind(c, name = "ssub_int")
           integer*1 si1, i1
           integer*2 si2, i2
           integer*4 si4, i4
           integer*8 si8, i8
           si1 = si1 - 3
           si2 = si2 - 3
           si4 = si4 - 3
           si8 = si8 - 3
           return
       entry extsub_int(i1, i2, i4, i8) BIND(C, name = "sub_int")
           i1 = i1 + 3
           i2 = i2 + 3
           i4 = i4 + 3
           i8 = i8 + 3
       end subroutine sextsub_int

      subroutine sextsub_real(sr4, sr8) bind(c, name = "ssub_real")
           real*4   sr4, r4
           real*8   sr8, r8
           sr4 = sr4 / 2
           sr8 = sr8 / 2
           return
       entry extsub_real(r4, r8) BIND(C, name = "sub_real")
           r4 = r4 * 2
           r8 = r8 * 2
       end subroutine sextsub_real


       subroutine sextsub_log(sl1) bind(c, name = "ssub_log")
           logical*1 sl1, l1
           sl1 = .false.
           return
       entry extsub_log(l1) BIND(C, name = "sub_log")
           l1 = .true.
       end subroutine sextsub_log


       subroutine sextsub_comp(sco8, sco16) bind(c, name = "ssub_comp")
           complex*8   sco8, co8
           complex*16  sco16, co16
           sco8 = (0.0, 0.0)
           sco16 = (0.0D0, 0.0D0)
           return
       entry extsub_comp(co8, co16) BIND(C, name = "sub_comp")
           co8 = (1.0, 1.0)
           co16 = (1.0D0, 1.0D0)
       end subroutine sextsub_comp


       subroutine sextsub_char(sch1) bind(c, name = "ssub_char")
           character*1 sch1, ch1
           sch1 = 'a'
           return
       entry extsub_char(ch1) BIND(C, name = "sub_char")
           ch1 = 'd'
       end subroutine sextsub_char

 end module m
