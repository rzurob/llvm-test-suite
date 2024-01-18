! *********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: EXEC_REP=1; $TR_SRC/fxbindc.sh fxbind_c01y bind_c01y
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
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : fxbind_c01y.f
!* TEST CASE TITLE              : BIND(C) attribute
!*
!* PROGRAMMER                   : Yubin Liao
!* DATE                         : Jan. 1, 2004
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf90
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test: BINC(C) attribute 
!*                                with different intrinsic data type,
!*                                integer*1, logical*1
!*                                character(1). Using external
!*                                subroutine,interface.Fortran calls C.
!*                                The arguments are passed by typeless
!*                                constants.
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/05/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxbind_c01a
   implicit none

   interface
       subroutine extsub_int(i1, i2) bind(c) 
           integer*1 i1
           integer*1 i2
       end subroutine extsub_int


       subroutine extsub_log(l1, l2) bind(c) 
           logical*1 l1
           logical*1 l2
       end subroutine extsub_log

       subroutine extsub_char(ch1, ch2) bind(c) 
           character*1 ch1
           character*1 ch2
       end subroutine extsub_char

   end interface

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

   call extsub_int(ai1, b'00001111' )
      if(ai1 .ne. bi1)then
        error stop 10
      endif


!**********************************************************
!        Calling C from Fortran with logical data type
!                and check the Results
!**********************************************************

   call extsub_log(al1, b'00000000')
      if(al1 .neqv. bl1)then
        error stop 30
      endif


!**********************************************************
!        Calling C from Fortran with character data type
!                and check the Results
!**********************************************************

   call extsub_char(ach1, x'61')
      if(ach1 .ne. bch1)then
        error stop 50
      endif
end 
