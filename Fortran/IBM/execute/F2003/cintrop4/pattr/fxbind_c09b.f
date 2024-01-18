! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: EXEC_REP=1; $TR_SRC/fxbindc.sh fxbind_c09b bind_c09a
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
!*                                with different intrinsic data type,
!*                                integer*1, integer*2, integer*4,
!*                                integer*8, real*4, real*8, complex
!*                                character(1). Using module
!*                                subroutine,interface.Fortran calls C.
!*                                C code is RECURSIVE.
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
       subroutine extsub_int(i1, i2, i4, i8, i) BIND(C)
           integer*1 i1
           integer*2 i2
           integer*4 i4,i
           integer*8 i8
       end subroutine extsub_int

       subroutine extsub_real(r4, r8, r) BIND(C)
           real*4   r4
           real*8   r8
           integer*4 r
       end subroutine extsub_real

       subroutine extsub_char(ch) bind(c)
           character ch
       end subroutine extsub_char

       subroutine extsub_comp(co8, co16, i) BIND(C)
           complex*8   co8
           complex*16  co16
           integer*4 i
       end subroutine extsub_comp

   end interface
end module m

   use m

   logical precision_R4, precision_R6, precision_R8
   logical precision_x8, precision_x16

   integer*1 ai1 /5/, bi1 /8/
   integer*2 ai2 /15/, bi2 /18/
   integer*4 ai4 /11/, bi4 /14/
   integer*8 ai8 /17/, bi8 /20/

   real*4   ar4 /4.80/, br4 /1.80/
   real*8   ar8 /140.8/, br8 /137.8/
   real*16  ar16 /1600.3/, br16 /1597.3/

   character ch /'f'/

   complex*8 co8 /(3.0, 6.0)/
   complex*16 co16 /(3.0D0, 6.0D0)/

   integer a
   a = 3

   call extsub_int(ai1, ai2, ai4, ai8, a)
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

   a = 3

   call extsub_real(ar4, ar8, a)

      if(.not. precision_R4(ar4,br4))then
        error stop 20
      end if

      if(.not. precision_R8(ar8,br8))then
        error stop 21
      end if


   call extsub_char(ch)
      if ( ch .ne. 'd') then
        error stop 30
      end if

   a = 3

   call extsub_comp(co8, co16, a)
      if ( .not. precision_x8(co8, (0.0,0.0))) then
        error stop 40
      end if

     if (.not. precision_x16(co16, (0.0D0, 0.0D0))) then
        error stop 41
     end if
end
