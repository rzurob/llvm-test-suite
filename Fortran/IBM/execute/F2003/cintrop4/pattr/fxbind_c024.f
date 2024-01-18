! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: EXEC_REP=1; $TR_SRC/fxbindc.sh fxbind_c024 bind_c01y
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
!* DATE                         : Jan. 1, 2004
!* ORIGIN                       : AIX Complier Development
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
!*                                subroutine interface. Fortran calls C.
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
   implicit none

   interface
       subroutine sub_int(i1, i2) bind(c, name = "extsub_int")
           integer*1 i1
           integer*1 i2
       end subroutine sub_int


       subroutine sub_log(l1, l2) bind(c, name = "extsub_log")
           logical*1 l1
           logical*1 l2
       end subroutine sub_log

       subroutine sub_char(ch1, ch2) bind(c, name = "extsub_char")
           character*1 ch1
           character*1 ch2
       end subroutine sub_char

   end interface
end module m

   use m
   logical precision_R4, precision_R6, precision_R8
   logical precision_x8, precision_x16

!**********************************************************
!        Initialization of variables                      *
!**********************************************************

   integer*1 ai1, bi1

   logical*1 al1 , bl1

   character*1 ach1 , bch1

   ai1 =  0
   bi1 =  15

   al1 = .true.
   bl1 = .false.

   ach1 = 'd'
   bch1 = 'a'


!**********************************************************
!        Calling C from Fortran with integer data type
!                and check the results
!**********************************************************

   call sub_int(ai1, b'00001111' )
      if(ai1 .ne. bi1)then
        error stop 10
      endif


!**********************************************************
!        Calling C from Fortran with logical data type
!                and check the Results
!**********************************************************

   call sub_log(al1, b'00000000')
      if(al1 .neqv. bl1)then
        error stop 30
      endif


!**********************************************************
!        Calling C from Fortran with character data type
!                and check the Results
!**********************************************************

   call sub_char(ach1, x'61')
      if(ach1 .ne. bch1)then
        error stop 50
      endif
end
