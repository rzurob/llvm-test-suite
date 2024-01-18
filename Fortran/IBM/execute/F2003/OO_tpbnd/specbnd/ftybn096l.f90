!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
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

      type parent
         integer :: x
      contains
      	 procedure, pass(arg5) :: bind_b => proc1
         procedure, pass :: bind_r => proc2
      end type

      type, extends(parent) :: child
      contains
         procedure, pass(arg2) :: bind_c => proc1
      end type

      type, extends(child) :: thirGen
      contains
         procedure, pass(arg4) :: bind_3 => proc1
      end type

      type, extends(thirGen) :: fourGen
      contains
         procedure, pass(arg1) :: bind_4 => proc1
      end type

      type, extends(fourGen) :: fifGen
      contains
         procedure, pass(arg3) :: bind_5 => proc1
      end type

      contains
      subroutine proc1(arg1, arg2, arg3, arg4, arg5)
         class(fourGen) :: arg1
         class(child)  :: arg2
         class(fifGen) :: arg3
         class(thirGen) :: arg4
         class(parent) :: arg5
         int = 400
         c = "hi_again"
      end subroutine

      subroutine proc2(arg1)
         class(parent) :: arg1
         int = 0
         c = ""
      end subroutine
   end module
   use mod

   type(parent) :: dt_p
   type(child) :: dt_c
   type(thirGen) :: dt_3
   type(fourGen) :: dt_4
   type(fifGen) :: dt_5

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

