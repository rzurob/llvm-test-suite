! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_tpbnd/specbnd/ftybn096e.f
! opt variations: -ql -qreuse=none

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : pass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : inheritance
!*
!*  DESCRIPTION                : parent procedures are inherited.
!*                               inherite from a different scroping unit.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      integer :: int = 200
      character*20 :: c = "hi"

      type parent(k1)    ! (4)
         integer, kind :: k1
         integer(k1)   :: x
	 contains
      	 procedure, pass :: bind => proc1
      end type

      contains
      subroutine proc1(arg1)
         class(parent(4)) :: arg1
         int = 400
         c = "hi_again"
      end subroutine
   end module

   module mod2
   use mod1
   type, extends(parent) :: child    ! (4)
      integer(k1) :: y
   end type
   end module

   use mod2

   type(parent(4)) :: dt_p
   type(child(4)) :: dt_c
   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3
   call dt_p%bind()
   if (int .ne. 400)      error stop 4
   if (c .ne. "hi_again")    error stop 5
   int = 0
   c = ""
   call dt_c%bind()
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7

   end
