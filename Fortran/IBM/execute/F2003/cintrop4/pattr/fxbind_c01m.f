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
!* DESCRIPTION                  : Test: entry BINC(C) attribute
!*                                with different intrinsic data type,
!*                                integer*1, integer*2, integer*4,
!*                                integer*8, real*4, real*8, real*16,
!*                                byte, character(1). Using external
!*                                entry. C calls Fortran.
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/05/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890


       subroutine sextsub_int(si1, si2, si4, si8)
           integer*1 si1
           integer*2 si2
           integer*4 si4
           integer*8 si8
           integer*1 i1
           integer*2 i2
           integer*4 i4
           integer*8 i8
           si1 = si1 + 31
           si2 = si2 + 31
           si4 = si4 + 31
           si8 = si8 + 31
       entry extsub_int(i1, i2, i4, i8) BIND(C)
           i1 = i1 + 3
           i2 = i2 + 3
           i4 = i4 + 3
           i8 = i8 + 3
       end subroutine sextsub_int


       subroutine sextsub_real(sr4, sr8)
           real*4   sr4, r4
           real*8   sr8, r8
           sr4 = sr4 * 21
           sr8 = sr8 * 21
       entry extsub_real(r4, r8, r16) BIND(C)
           r4 = r4 * 2
           r8 = r8 * 2
       end subroutine sextsub_real


       subroutine sextsub_log(sl1)
           logical*1 sl1, l1
           sl1 = .false.
       entry extsub_log(l1) BIND(C)
           l1 = .true.
       end subroutine sextsub_log


       subroutine sextsub_comp(sco8, sco16)
           complex*8   sco8, co8
           complex*16  sco16, co16
           sco8 = (10.0, 10.0)
           sco16 = (10.0D0, 10.0D0)
       entry extsub_comp(co8, co16) BIND(C)
           co8 = (1.0, 1.0)
           co16 = (1.0D0, 1.0D0)
       end subroutine sextsub_comp


       subroutine sextsub_char(sch1)
           character*1 sch1, ch1
           sch1 = 'e'
       entry extsub_char(ch1) BIND(C)
           ch1 = 'd'
       end subroutine sextsub_char

