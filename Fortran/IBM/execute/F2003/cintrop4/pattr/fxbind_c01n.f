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
!* DESCRIPTION                  : Test: entry BINC(C) attribute
!*                                with derived types with different
!*                                intrinsic data type.
!*                                Fortran calls C.
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




       subroutine sextsub_der(sder)
         use m
         type(der_bind) :: sder, der

         sder%i1 = sder%i1 + 31
         sder%i2 = sder%i2 + 31
         sder%i4 = sder%i4 + 31
         sder%i8 = sder%i8 + 31

         sder%r4 = sder%r4 * 21
         sder%r8 = sder%r8 * 21

         sder%ch1 = 'a'

         sder%l1 = .false.

         sder%x8 = (60.0, 80.0)
         sder%x16 = (100.0D0, 120.0D0)

       entry extsub_der(der) BIND(C)


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


