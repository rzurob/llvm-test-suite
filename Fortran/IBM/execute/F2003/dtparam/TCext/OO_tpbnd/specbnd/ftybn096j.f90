! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specbnd/ftybn096j.f
! opt variations: -qnol

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : overriding
!*
!*  DESCRIPTION                : testing a procedure is bound to both
!*                                parent and child types,
!*                                but with different binding-names.
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
      	 procedure, pass(arg3) :: bind => proc1
         procedure, pass :: bind_r => proc2
      end type

      type, extends(parent) :: child    ! (20,4)
      contains
         procedure, pass(arg2) :: bind_c => proc1
      end type

      type, extends(child) :: thirGen    ! (20,4)
      contains
         procedure, pass :: bind_3g => proc1
      end type

      contains
      subroutine proc1(arg1, arg2, arg3)
         class(thirGen(*,4)) :: arg1
         class(child(*,4)) :: arg2
         class(parent(*,4)) :: arg3
         int = 400
         c = "hi_again"
      end subroutine

      subroutine proc2(arg1, arg2, arg3)
         class(parent(*,4)) :: arg1
         class(child(*,4)) :: arg2
         class(thirGen(*,4)) :: arg3
         int = 0
         c = "hello"
      end subroutine
   end module

   use mod

   type(parent(20,4)) :: dt_p
   type(child(20,4)) :: dt_c
   type(thirGen(20,4)) :: dt_3g
   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3

   call dt_p%bind(dt_3g, dt_c)
   if (int .ne. 400)      error stop 4
   if (c .ne. "hi_again")    error stop 5
   call dt_p%bind_r(dt_c, dt_3g)

   call dt_c%bind_c(dt_3g, dt_p)
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7
   call dt_c%bind_r(dt_c, dt_3g)

   call dt_3g%bind_3g(dt_c, dt_p)
   if (int .ne. 400)      error stop 8
   if (c .ne. "hi_again")    error stop 9
   call dt_3g%bind_r(dt_c, dt_3g)
   if (int .ne. 0) error stop 10
   if (c .ne. "hello") error stop 11

   end

