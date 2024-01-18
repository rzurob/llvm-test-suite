! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: EXEC_REP=1; $TR_SRC/fxbindc.sh fxbind_c01k bind_c01k
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
!* DATE                         : Sep. 1, 2003
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test: BINC(C) attribute
!*                                with derived types with different
!*                                intrinsic data type
!*                                Using external subroutine,interface.
!*                                C calls fortran, with binding lables.
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
         character*1  ch1
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




       subroutine extsub_der(der) bind(c, name = "sub_der")
         use m
         type(der_bind) :: der

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

       end subroutine extsub_der



