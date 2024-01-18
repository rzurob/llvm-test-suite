! *********************************************************************
!**********************************************************************
!* ===================================================================
!*
!* DATE                         : Jan. 1, 2003
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test: BINC(C) attribute/statement
!*                                with different intrinsic data type,
!*                                integer*1, integer*2, integer*4,
!*                                integer*8, real*4, real*8, real*16,
!*                                byte, character(1). Using module
!*                                recursive entry
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
       recursive subroutine sextsub_int(i1, i2, i4, i8, i)
           integer*1 i1
           integer*2 i2
           integer*4 i4,i
           integer*8 i8
           i1 = i1 + 1
           i2 = i2 + 1
           i4 = i4 + 1
           i8 = i8 + 1
       entry extsub_int(i1, i2, i4, i8, i) bind(c)
           if ( i <= 0) then
             return
           end if
           i1 = i1 + 1
           i2 = i2 + 1
           i4 = i4 + 1
           i8 = i8 + 1
           i  = i  - 1
           call extsub_int(i1, i2, i4, i8, i)
       end subroutine sextsub_int

       recursive subroutine sextsub_real(r4, r8, r)
           real*4   r4
           real*8   r8
           integer*4 r
           r4 = r4 - 1
           r8 = r8 - 1
       entry extsub_real(r4, r8, r) bind(c)
           if (r <= 0 ) then
             return
           end if
           r4 = r4 - 1
           r8 = r8 - 1
           r  = r  - 1
           call extsub_real(r4, r8, r)
       end subroutine sextsub_real

       recursive subroutine sextsub_char(ch, i)
           character ch
           integer*4 i
           return
       entry extsub_char(ch, i) bind(c)
           if ( i > 0) then
              i = i - 1
              call extsub_char(ch, i)
           else
              ch = 'd'
           end if
        end subroutine sextsub_char

       recursive subroutine sextsub_comp(co8, co16, i)
           complex*8   co8
           complex*16  co16
           integer*4 i
       entry extsub_comp(co8, co16, i) bind(c)
           if ( i <= 0 ) then
             return
           end if
           co8 = co8 - (1.0, 2.0)
           co16 = co16 - (1.0d0, 2.0D0)
           i = i - 1
           call extsub_comp(co8, co16, i)
       end subroutine sextsub_comp
end module m
