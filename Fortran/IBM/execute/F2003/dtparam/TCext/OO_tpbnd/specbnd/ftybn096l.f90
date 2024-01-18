! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specbnd/ftybn096l.f
! opt variations: -qnol

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod 
! %COMPOPTS: -qfree=f90 
! %GROUP: ftybn096l.f 
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: 
! %END
!**********************************************************************
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn096l.f 
!*  TEST CASE TITLE            : type-bound procedure
!*
!*  PROGRAMMER                 : Catherine Sun
!*  DATE                       : 
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : pass binding attribute 
!*
!*  SECONDARY FUNCTIONS TESTED : inheritance
!*
!*  DESCRIPTION                : testing the base procedure is bound to
!*                               multiple level inherited types with 
!*                               different binding-names. 
!*    
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod	      
      integer :: int = 200
      character*20 :: c = "hi"

      type parent(n1,k1)    ! (20,4) 
         integer, kind :: k1
         integer, len  :: n1
         integer(k1)   :: x
      contains
      	 procedure, pass(arg5) :: bind_b => proc1
         procedure, pass :: bind_r => proc2
      end type 

      type, extends(parent) :: child    ! (20,4) 
      contains
         procedure, pass(arg2) :: bind_c => proc1
      end type

      type, extends(child) :: thirGen    ! (20,4)
      contains
         procedure, pass(arg4) :: bind_3 => proc1
      end type

      type, extends(thirGen) :: fourGen    ! (20,4)
      contains
         procedure, pass(arg1) :: bind_4 => proc1
      end type

      type, extends(fourGen) :: fifGen    ! (20,4) 
      contains
         procedure, pass(arg3) :: bind_5 => proc1
      end type

      contains
      subroutine proc1(arg1, arg2, arg3, arg4, arg5)
         class(fourGen(*,4)) :: arg1
         class(child(*,4))  :: arg2
         class(fifGen(*,4)) :: arg3
         class(thirGen(*,4)) :: arg4
         class(parent(*,4)) :: arg5
         int = 400
         c = "hi_again"
      end subroutine

      subroutine proc2(arg1)
         class(parent(*,4)) :: arg1 
         int = 0
         c = ""
      end subroutine
   end module     
   use mod

   type(parent(20,4)) :: dt_p
   type(child(20,4)) :: dt_c
   type(thirGen(20,4)) :: dt_3
   type(fourGen(20,4)) :: dt_4
   type(fifGen(20,4)) :: dt_5

   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3

   call dt_p%bind_b(dt_4, dt_c, dt_5, dt_3)
   if (int .ne. 400)      error stop 4
   if (c .ne. "hi_again")    error stop 5
   call dt_p%bind_r()

   call dt_c%bind_c(dt_4, dt_5, dt_3, dt_p)
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7
   call dt_c%bind_r()

   call dt_3%bind_3(dt_4, dt_c, dt_5, dt_p)
   if (int .ne. 400)      error stop 8
   if (c .ne. "hi_again")    error stop 9
   call dt_3%bind_r()

   call dt_4%bind_4(dt_c, dt_5, dt_3, dt_p)
   if (int .ne. 400)      error stop 12
   if (c .ne. "hi_again")    error stop 13
   call dt_4%bind_r()

   call dt_5%bind_5(dt_4, dt_c, dt_3, dt_p)
   if (int .ne. 400)      error stop 16
   if (c .ne. "hi_again")    error stop 17

   end
   
