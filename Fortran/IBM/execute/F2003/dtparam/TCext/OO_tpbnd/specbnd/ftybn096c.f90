! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_tpbnd/specbnd/ftybn096c.f
! opt variations: -ql

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod 
! %COMPOPTS: -qfree=f90 
! %GROUP: ftybn096c.f 
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
!*  TEST CASE NAME             : ftybn0926c.f 
!*  TEST CASE TITLE            : type-bound procedure
!*
!*  PROGRAMMER                 : Catherine Sun
!*  DATE                       : 
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : pass binding attribute 
!*
!*  SECONDARY FUNCTIONS TESTED : inheritence 
!*
!*  DESCRIPTION                : parent procedures are inherited.
!*                               with two levels inheritance. 
!*    
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod	      
      integer :: int = 200
      character*20 :: c = "hi"

      type parent(k1)    ! (4) 
         integer, kind :: k1
         integer(k1)   :: x
         contains
         procedure, pass :: bind => proc1
      end type 

      type, extends(parent) :: child    ! (4) 
      end type

      type, extends(child) :: thirdGen    ! (4)
      end type

      contains
      subroutine proc1(arg1)
         class(parent(4)) :: arg1
         int = 400
         c = "hi_again"
      end subroutine

   end module     

   use mod

   type(parent(4)) :: dt_p
   type(child(4)) :: dt_c
   type(thirdGen(4)) :: dt_3g
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

   int = 0
   c = ""
   call dt_3g%bind()
   if (int .ne. 400)      error stop 8
   if (c .ne. "hi_again")    error stop 9
   end
   
