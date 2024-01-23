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
!* DESCRIPTION                  : Test: subroutine with BINd(C) attribute
!*                                with different pointer intrinsic data type,
!*                                integer*1, integer*2, integer*4,
!*                                integer*8, real*4, real*8, real*16,
!*                                byte, character(1). Using external
!*                                subroutine,interface.Fortran calls C.
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
       subroutine extsub_int(i1, i2, i4, i8) bind(c)
           integer*1 i1
           integer*2 i2
           integer*4 i4
           integer*8 i8
       end subroutine extsub_int

       subroutine extsub_real(r4, r8) bind(c)
           real*4   r4
           real*8   r8
       end subroutine extsub_real

       subroutine extsub_log(l1) bind(c)
           logical*1 l1
           !logical*2 l2
           !logical*4 l4
           !logical*8 l8
       end subroutine extsub_log

       subroutine extsub_comp(co8, co16) bind(c)
           complex*8   co8
           complex*16  co16
       end subroutine extsub_comp

       subroutine extsub_char(ch1) bind(c)
           character*1 ch1
       end subroutine extsub_char

   end interface

   logical precision_R4, precision_R6, precision_R8
   logical precision_x8, precision_x16

!**********************************************************
!        Initialization of variables                      *
!**********************************************************
   integer*1, pointer :: ai1
   integer*2, pointer :: ai2
   integer*4, pointer :: ai4
   integer*8, pointer :: ai8

   real*4, pointer ::   ar4
   real*8, pointer ::   ar8

   logical*1, pointer :: al1

   complex*8, pointer :: ac8
   complex*16, pointer :: ac16

   character*1, pointer ::  ach1

   integer*1, target :: tai1 /5/, bi1 /8/
   integer*2, target :: tai2 /15/, bi2 /18/
   integer*4, target :: tai4 /11/, bi4 /14/
   integer*8, target :: tai8 /17/, bi8 /20/

   real*4, target ::   tar4 /4.80/, br4 /9.6/
   real*8, target ::   tar8 /140.8D0/, br8 /281.6D0/

   logical*1, target :: tal1 /.false./, bl1 /.true./

   complex*8, target ::   tac8 /(0.0, 0.0)/, bc8 /(1.0, 1.0)/
   complex*16, target ::  tac16 /(0.0D0, 0.0D0)/, bc16 /(1.0D0, 1.0D0)/

   character*1, target :: tach1 /'a'/, bch1 /'d'/

   ai1 => tai1
   ai2 => tai2
   ai4 => tai4
   ai8 => tai8
   ar4 => tar4
   ar8 => tar8
   al1 => tal1
   ac8 => tac8
   ac16=> tac16
   ach1=> tach1

!**********************************************************
!        Calling C from Fortran with integer data type
!                and check the results
!**********************************************************

   call extsub_int(ai1, ai2, ai4, ai8)
      if(ai1 .ne. bi1)then
        error stop 10
      endif

      if(ai2 .ne. bi2)then
        error stop 11
      endif

      if(ai4 .ne. bi4)then
        error stop 12
      endif

      if(ai8 .ne. bi8)then
        error stop 13
      endif

!**********************************************************
!        Calling C from Fortran with real data type
!                and check the Results
!**********************************************************

   call extsub_real(ar4, ar8)

      if(.not. precision_R4(ar4,br4))then
        error stop 20
      endif

      if(.not. precision_R8(ar8,br8))then
        error stop 21
      endif


!**********************************************************
!        Calling C from Fortran with logical data type
!                and check the Results
!**********************************************************

   call extsub_log(al1)
      if(al1 .neqv. bl1)then
        error stop 30
      endif


!**********************************************************
!        Calling C from Fortran with character data type
!                and check the Results
!**********************************************************

   call extsub_char(ach1)
      if(ach1 .ne. bch1)then
        error stop 50
      endif

!**********************************************************
!        Calling C from Fortran with Complex data type
!                and check the Results
!**********************************************************
   call extsub_comp(ac8, ac16)

      if(.not. precision_x8(ac8,bc8))then
        error stop 61
      endif

      if(.not. precision_x16(ac16,bc16))then
        error stop 62
      endif

end
