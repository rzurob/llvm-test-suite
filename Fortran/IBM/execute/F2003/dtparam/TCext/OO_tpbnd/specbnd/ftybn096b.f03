! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specbnd/ftybn096b.f
! opt variations: -qnol

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : pass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : parent procedures are inherited.
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
            procedure, pass :: bind => proc1
         end type

      type, extends(parent) :: child1    ! (20,4)
      end type

      type, extends(parent) :: child2    ! (20,4)
      end type


      contains
      subroutine proc1(arg1)
         class(parent(*,4)) :: arg1
         int = 400
         c = "hi_again"
      end subroutine
   end module

   use mod

   type(parent(20,4)) :: dt_p
   type(child1(20,4)) :: dt_c1
   type(child2(20,4)) :: dt_c2
   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3
   call dt_p%bind()
   if (int .ne. 400)      error stop 4
   if (c .ne. "hi_again")    error stop 5
   int = 0
   c = ""
   call dt_c1%bind()
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7
   int = 0
   c = ""
   call dt_c1%bind()
   if (int .ne. 400)      error stop 8
   if (c .ne. "hi_again")    error stop 9


   end


