! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_tpbnd/specbnd/ftybn020h.f
! opt variations: -ql

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : private type bound procedure
!*
!*  SECONDARY FUNCTIONS TESTED : nopass, non_overridable
!*
!*  DESCRIPTION                : the accessiblity of a type-bound procedure
!*                               is not affected by a PRIVATE statement
!*                               in the component-part, the accessiblity
!*                               of a data component is not affected by a
!*                               PRIVATE statemnt in the type-bound-procedure
!*                               -part.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      type parent(k1)    ! (4)
         integer, kind :: k1
         integer(k1)   :: x
	 contains
         private
      	 procedure, nopass, non_overridable :: bind => proc1
      end type

      type, extends(parent) :: child    ! (4)
      end type

      type(parent(4)) :: dt_p
      type(child(4)) :: dt_c

      contains
      subroutine proc1(arg1)
         class(parent(4)) :: arg1
         arg1%x = 100
      end subroutine

   subroutine test1
      call dt_p%bind(dt_p)
      call dt_c%bind(dt_c)
   end subroutine

   end module

   use mod1

   call test1()

   if (dt_p%x .ne. 100)  error stop 2_4
   if (dt_c%x .ne. 100)  error stop 3_4

   end
