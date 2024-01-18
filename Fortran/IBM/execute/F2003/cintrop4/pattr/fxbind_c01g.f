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
!*                                subroutine,interface.C calls Fortran.
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/05/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890


       subroutine extsub_int(i1, i2, i4, i8) bind(c)
           integer*1 i1
           integer*2 i2
           integer*4 i4
           integer*8 i8
           i1 = i1 + 3
           i2 = i2 + 3
           i4 = i4 + 3
           i8 = i8 + 3
       end subroutine extsub_int

       subroutine extsub_real(r4, r8) bind(c)
           real*4   r4
           real*8   r8
           r4 = r4 * 2
           r8 = r8 * 2
       end subroutine extsub_real

       subroutine extsub_log(l1) bind(c)
           logical*1 l1
           l1 = .true.
       end subroutine extsub_log

       subroutine extsub_comp(co8, co16) bind(c)
           complex*8   co8
           complex*16  co16
           co8 = (1.0, 1.0)
           co16 = (1.0D0, 1.0D0)
       end subroutine extsub_comp

       subroutine extsub_char(ch1) bind(c)
           character*1 ch1
           ch1 = 'd'
       end subroutine extsub_char


