! *********************************************************************
!**********************************************************************
!* ===================================================================
!*
!* DATE                         : Sep. 1, 2003
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test: BINC(C) attribute with binding labels
!*                                with derived types with different
!*                                intrinsic data type
!*                                Using external subroutine, entry.
!*                                Both subroutine and entry have
!*                                bind(c) attribute.
!*                                C calls Fortran.
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/05/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type, bind(c) :: der_bind

         character*1 ch1

         logical*1 l1

         integer*1 i1
         integer*2 i2
         integer*4 i4
         integer*8 i8

         real*4 r4
         real*8 r8

         complex*8 x8
         complex*16 x16
    end type der_bind
end module




       subroutine sextsub_der(sder) bind(c, name = "ssub_der")
         use m
         type(der_bind) :: sder, der

         sder%i1 = sder%i1 - 3
         sder%i2 = sder%i2 - 3
         sder%i4 = sder%i4 - 3
         sder%i8 = sder%i8 - 3

         sder%r4 = sder%r4 / 2
         sder%r8 = sder%r8 / 2

         sder%ch1 = 'a'

         sder%l1 = .false.

         sder%x8 = (3.0, 4.0)
         sder%x16 = (5.0D0, 6.0D0)
         return
       entry extsub_der(der) bind(c, name = "sub_der")


         der%i1 = der%i1 + 3
         der%i2 = der%i2 + 3
         der%i4 = der%i4 + 3
         der%i8 = der%i8 + 3

         der%r4 = der%r4 * 2
         der%r8 = der%r8 * 2

         der%ch1 = 'd'

         der%l1 = .true.

         der%x8 = (6.0, 8.0)
         der%x16 = (10.0D0, 12.0D0)

       end subroutine sextsub_der



