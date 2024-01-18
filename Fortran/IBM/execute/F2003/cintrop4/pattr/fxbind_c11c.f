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
!*                                with compatible attribute - internt(in)
!*                                intent(out), target,
!*                                integer*1, integer*2, integer*4,
!*                                integer*8, real*4, real*8, complex
!*                                character(1). Using external
!*                                subroutine. C calls Fortran.
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/05/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890




       subroutine extsub_int(i1, i2, i4, i8, i81) BIND(C)
           integer*1, intent(in), target  :: i1
           integer*2, intent(in), target  :: i2
           integer*4, intent(in), target  :: i4
           integer*8, intent(in), target  :: i8
           integer*8, intent(out) ::  i81
           i81 = i1 + i2 + i4 + i8
       end subroutine extsub_int

       subroutine extsub_real(r4, r8, r161) BIND(C)
           real*4, intent(in), target   ::  r4
           real*8, intent(in), target   ::  r8
           real*8, intent(out) :: r161
           r161 = r4 + r8
       end subroutine extsub_real

       subroutine extsub_log(l1, l11) BIND(C)
           logical*1, intent(in), target :: l1
           logical*1, intent(out) ::  l11
           l11 = l1
       end subroutine extsub_log

       subroutine extsub_comp(co8, co16, co81, co161) BIND(C)
           complex*8, intent(in), target   ::  co8
           complex*16, intent(in), target  ::  co16
           complex*8, intent(out) ::  co81
           complex*16, intent(out) :: co161
           co81 = co8
           co161 = co16
       end subroutine extsub_comp

       subroutine extsub_char(ch1, ch11) BIND(C)
           character*1, intent(in), target  :: ch1
           character*1, intent(out) ::  ch11
           ch11 = ch1
       end subroutine extsub_char

