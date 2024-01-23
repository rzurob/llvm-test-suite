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
!*                                integer*1, logical*1
!*                                character(1). Using module
!*                                entry. C calls Fortran.
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
       subroutine sextsub_int(i1, i2)
           integer*1 i1
           integer*8 i2
           return
       entry bextsub_int(i1, i2) bind(c, name = "extsub_int")
           i1 = b'00001111'
           i2 = x'000000000000000F'
       end subroutine sextsub_int


       subroutine sextsub_log(l1)
           logical*1 l1
           return
       entry bextsub_log(l1) bind(c, name = "extsub_log")
           l1 = b'00000001'
       end subroutine sextsub_log

       subroutine sextsub_char(ch1)
           character*1 ch1
           return
       entry bextsub_char(ch1) bind(c, name = "extsub_char")
           ch1 = x'61'
       end subroutine sextsub_char
end module m
