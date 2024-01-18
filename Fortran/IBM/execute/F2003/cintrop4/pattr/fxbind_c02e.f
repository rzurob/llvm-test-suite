! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: EXEC_REP=1; $TR_SRC/fxbindc.sh fxbind_c02e bind_c01e
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
!*                                with derived type which consists of
!*                                different
!*                                intrinsic data type
!*                                Using module subroutine,interface.
!*                                Fortran calls C. with binding lables.
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

         character*1 c

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
end module m

module act
   use m
   implicit none

   interface
       subroutine extsub_der(der) bind(c, name = "sub_der")
         use m
         type(der_bind) :: der
       end subroutine extsub_der
   end interface
end module act

        use act
        logical precision_R4, precision_R6, precision_R8
        logical precision_x8, precision_x16

        type(der_bind)  der_type, d_t


!**********************************************************
!        Initialization of variables                      *
!**********************************************************

           der_type%i1 = 5
           d_t%i1 = 8
           der_type%i2 = 15
           d_t%i2 = 18
           der_type%i4 = 11
           d_t%i4 = 14
           der_type%i8 = 17
           d_t%i8 = 20

           der_type%r4 = 4.80
           d_t%r4 = 9.6
           der_type%r8 = 140.8D0
           d_t%r8 = 281.6D0

           der_type%l1 = .false.
           d_t%l1 = .true.

           der_type%c = 'a'
           d_t%c = 'd'

           der_type%x8 = (3.0,4.0)
           der_type%x16 = (5.0D0,6.0D0)

           d_t%x8 = (6.0,8.0)
           d_t%x16 = (10.0D0,12.0D0)



!**********************************************************
!Calling external subroutine with different data type in
! deriver type which will call C and check the results
!**********************************************************

           call extsub_der(der_type)
           if(der_type%i1 .ne. d_t%i1) error stop 10
           if(der_type%i2 .ne. d_t%i2) error stop 11
           if(der_type%i4 .ne. d_t%i4) error stop 12
           if(der_type%i8 .ne. d_t%i8) error stop 13

           if(.not. precision_r4(der_type%r4, d_t%r4)) error stop 14
           if(.not. precision_r8(der_type%r8, d_t%r8)) error stop 15

           if(der_type%c .ne. d_t%c) error stop 17

           if(der_type%l1 .neqv. .true.) error stop 18

           if(.not. precision_x8(der_type%x8, d_t%x8)) error stop 19
           if(.not. precision_x16(der_type%x16, d_t%x16)) error stop 20

end
