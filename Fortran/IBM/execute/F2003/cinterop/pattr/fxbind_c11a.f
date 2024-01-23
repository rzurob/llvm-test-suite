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
!*                                integer*1, integer*2, integer*4,
!*                                integer*8, real*4, real*8,
!*                                character(1) with attribute target,
!*                                intent(in) and intent(out). Using external
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
           integer*1, intent(in), target :: i1
           integer*2, intent(in), target :: i2
           integer*4, intent(in), target :: i4
           integer*8, intent(in), target :: i8
           integer*8, intent(out) :: i81
       end subroutine extsub_int

       subroutine extsub_real(r4, r8, r161) BIND(C)
           real*4, intent(in), target   ::  r4
           real*8, intent(in), target   ::  r8
           real*8, intent(out) ::  r161
       end subroutine extsub_real

       subroutine extsub_log(l1, l11) BIND(C)
           logical*1, intent(in), target :: l1
           logical*1, intent(out), target :: l11
       end subroutine extsub_log

       subroutine extsub_comp(co8, co16, co81, co161) BIND(C)
           complex*8, intent(in), target  ::  co8
           complex*16, intent(in), target ::  co16
           complex*8, intent(out) :: co81
           complex*16, intent(out) :: co161
       end subroutine extsub_comp

       subroutine extsub_char(ch1, ch11) BIND(C)
           character*1, intent(in), target :: ch1
           character*1, intent(out) ::  ch11
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
   integer*8 ai8 /17/, bi8 /17/, i811/0/, i812 /48/

   real*4   ar4 /4.80/, br4 /4.80/
   real*8   ar8 /40.0/, br8 /40.0/
   real*8  ar16 /600.0/, br16 /600.0/, r161/0.0D0/, r162

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
   call extsub_real(ar4, ar8, r161)

      if(.not. precision_R4(ar4,br4))then
        error stop 20
      endif

      if(.not. precision_R8(ar8,br8))then
        error stop 21
      endif


      if(.not. precision_R8(r161,r162))then
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

      !if(al2 .neqv. bl2)then
      !  error stop 31
      !endif

      !if(al4 .neqv. bl4)then
      !  error stop 32
      !endif

      !if(al8 .neqv. bl8)then
      !  error stop 33
      !endif

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

   !call extsub_byte(ab1)
   !   if(ab1 .ne. bb1)then
   !     error stop 60
   !   endif

end
