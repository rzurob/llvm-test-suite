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




       subroutine sextsub_int(i1, i2, i4, i8, i81)
           integer*1, value :: i1
           integer*2, value :: i2
           integer*4, value :: i4
           integer*8, value :: i8
           integer*8 i81
           return
       entry extsub_int(i1, i2, i4, i8, i81) bind(c)
           i81 = i1 + i2 + i4 + i8
           i1 = i1 + 3
           i2 = i2 + 3
           i4 = i4 + 3
           i8 = i8 + 3
       end subroutine sextsub_int

       subroutine sextsub_real(r4, r8, r161)
           real*4, value  ::  r4
           real*8, value  ::  r8
           real*8 r161
           return
       entry extsub_real(r4, r8, r161) bind(c)
           r161 = r4 + r8
           r4 = r4 * 2
           r8 = r8 * 2
       end subroutine sextsub_real

       subroutine sextsub_log(l1, l11)
           logical*1, value :: l1
           logical*1 l11
           l11 = .not. l1
           return
       entry extsub_log(l1, l11) bind(c)
           l11 = l1
           l1 = .true.
       end subroutine sextsub_log

       subroutine sextsub_comp(co8, co16, co81, co161)
           complex*8, value  ::  co8
           complex*16, value ::  co16
           complex*8 co81
           complex*16 co161
           return
       entry extsub_comp(co8, co16, co81, co161) bind(c)
           co81 = co8
           co161 = co16
           co8 = (1.0, 1.0)
           co16 = (1.0D0, 1.0D0)
       end subroutine sextsub_comp

       subroutine sextsub_char(ch1, ch11)
           character*1, value :: ch1
           character*1 ch11
           ch11 = 'u'
           return
       entry extsub_char(ch1, ch11) bind(c)
           ch11 = ch1
           ch1 = 'd'
       end subroutine sextsub_char

