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
!*                                with different intrinsic data type with
!*                                "value" attibute,
!*                                integer*1, integer*2, integer*4,
!*                                integer*8, real*4, real*8,
!*                                character(1), complex. Using external
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
       subroutine extsub_int(i1, i2, i4, i8, i81) BIND(C)
           integer*1, value :: i1
           integer*2, value :: i2
           integer*4, value :: i4
           integer*8, value :: i8
           integer*8 i81
       end subroutine extsub_int

       subroutine extsub_real(r4, r8, r81) BIND(C)
           real*4, value  ::  r4
           real*8, value  ::  r8
           real*8  r81
       end subroutine extsub_real

       subroutine extsub_log(l1, l11) BIND(C)
           logical*1, value :: l1
           logical*1 l11
       end subroutine extsub_log

       subroutine extsub_comp(co8, co16, co81, co161) BIND(C)
           complex*8, value  ::  co8
           complex*16, value ::  co16
           complex*8 co81
           complex*16 co161
       end subroutine extsub_comp

       subroutine extsub_char(ch1, ch11) BIND(C)
           character*1, value :: ch1
           character*1 ch11
       end subroutine extsub_char

   end interface

   logical precision_R4, precision_R6, precision_R8
   logical precision_x8, precision_x16

!**********************************************************
!        Initialization of variables                      *
!**********************************************************

   integer*1 ai1 /5/, bi1 /5/
   integer*2 ai2 /15/, bi2 /15/
   integer*4 ai4 /11/, bi4 /11/
   integer*8 ai8 /17/, bi8 /17/, i811, i812 /48/

   real*4   ar4 /4.80/, br4 /4.80/
   real*8   ar8 /40.0D0/, br8 /40.0D0/
   real*8  ar16 /600.0/, br16 /600.0/, r81, r162

   logical*1 al1 /.false./, bl1 /.true./, l11 /.false./
   !logical*2 al2 /.false./, bl2 /.true./
   !logical*4 al4 /.true./, bl4 /.false./
   !logical*8 al8  /.true./, bl8 /.false./

   complex*8   ac8 /(0.0, 0.0)/, bc8 /(1.0, 1.0)/
   complex*16  ac16 /(0.0D0, 0.0D0)/, bc16 /(1.0D0, 1.0D0)/

   character*1 ach1 /'a'/, bch1 /'a'/, ch11 /'b'/



!**********************************************************
!        Calling C from Fortran with integer data type
!                and check the results
!**********************************************************

   call extsub_int(ai1, ai2, ai4, ai8, i811)
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

      if(i811 .ne. i812) then
         error stop 14
      end if

!**********************************************************
!        Calling C from Fortran with real data type
!                and check the Results
!**********************************************************

   r162 = ar4 + ar8
   call extsub_real(ar4, ar8, r81)

      if(.not. precision_R4(ar4,br4))then
        error stop 20
      endif

      if(.not. precision_R8(ar8,br8))then
        error stop 21
      endif

      if(.not. precision_R8(r81, r162)) then
        error stop 22
      endif

!**********************************************************
!        Calling C from Fortran with logical data type
!                and check the Results
!**********************************************************

   call extsub_log(al1, l11)
      if(al1 .neqv. .false.)then
        error stop 30
      endif

      if(l11 .neqv. bl1) then
        error stop 31
      end if


!**********************************************************
!        Calling C from Fortran with character data type
!                and check the Results
!**********************************************************

   call extsub_char(ach1, ch11)
      if(ach1 .ne. bch1)then
        error stop 50
      endif

      if(ch11 .ne. 'd' )then
        error stop 51
      endif

!**********************************************************
!        Calling C from Fortran with Complex data type
!                and check the Results
!**********************************************************
   call extsub_comp(ac8, ac16, bc8, bc16)

      if(.not. precision_x8(ac8,bc8))then
        error stop 61
      endif

      if(.not. precision_x16(ac16,bc16))then
        error stop 62
      endif


end