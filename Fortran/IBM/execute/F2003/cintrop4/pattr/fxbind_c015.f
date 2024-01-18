! *********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: EXEC_REP=1; $TR_SRC/fxbindc.sh fxbind_c015 bind_c01z
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
!* TEST CASE TITLE              : fxbind_c015.f
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
!*                                with C_ptr type of integer, real
!*                                logical(1), character(1). Using external
!*                                subroutine,interface.Fortran calls C.
!*                                with binding labels.
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/05/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxbind_c01a
   use iso_c_binding

   implicit none

   interface
       subroutine sub_int(i1) bind(c, name = "extsub_int")
           use iso_c_binding 
           type(C_PTR) :: i1
       end subroutine sub_int
      
       subroutine sub_real(r4) bind(c, name = "extsub_real")
           use iso_c_binding
           type(c_ptr) :: r4
       end subroutine sub_real

       subroutine sub_log(l1) bind(c, name = "extsub_log") 
           use iso_c_binding
           type(C_PTR) ::  l1
       end subroutine sub_log

       subroutine sub_char(ch1) bind(c, name = "extsub_char") 
           use iso_c_binding
           type(C_PTR) ::  ch1
       end subroutine sub_char

   end interface


   logical precision_R4, precision_R6, precision_R8
   logical precision_x8, precision_x16
   
!**********************************************************
!        Initialization of variables                      *
!**********************************************************

   integer*1, target :: ai1, bi1 
   
   real*4, target :: ar4, br4

   logical*1, target ::  al1 , bl1 

   
   character*1, target :: ach1 , bch1 
   
   type(C_PTR) :: cp

   ai1 =  0
   bi1 =  15

   ar4 = 4.80
   br4 = 9.60

   al1 = .true.
   bl1 = .false.
   
   ach1 = 'd'
   bch1 = 'a'

 
!**********************************************************
!        Calling C from Fortran with integer data type
!                and check the results
!**********************************************************
   
   cp = c_loc(ai1)
 
   call sub_int(cp)
      if(ai1 .ne. bi1)then
        error stop 10
      endif

!**********************************************************
!        Calling C from Fortran with logical data type
!                and check the Results
!**********************************************************

   cp = c_loc(al1)

   call sub_log(cp)
      if(al1 .neqv. bl1)then
        error stop 30
      endif


!**********************************************************
!        Calling C from Fortran with character data type
!                and check the Results
!**********************************************************

   cp = c_loc(ach1)

   call sub_char(cp)
      if(ach1 .ne. bch1)then
        error stop 50
      endif

   cp = c_loc(ar4)
 
!**********************************************************
!        Calling C from Fortran with real data type
!                and check the Results
!**********************************************************

 
   call sub_real(cp)
      if (.not. precision_r4(ar4, br4)) then
         error stop 60
      end if  
      

end 
