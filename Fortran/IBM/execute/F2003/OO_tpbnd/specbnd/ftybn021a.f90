!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod 
! %COMPOPTS: -qfree=f90 
! %GROUP: ftybn021a.f 
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
!*  TEST CASE NAME             : ftybn021a.f 
!*  TEST CASE TITLE            : type-bound procedure
!*
!*  PROGRAMMER                 : Catherine Sun
!*  DATE                       : 
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute 
!*
!*  SECONDARY FUNCTIONS TESTED : overriding 
!*
!*  DESCRIPTION                : Testing the nopass binding attribute,
!*                               the binding procedures wit one dummy
!*                               argument & overriding in the same
!*                               scope.
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


	module mod	      
		type base 
      integer :: x
		contains
      procedure, nopass :: bind => proc1
		end type base

   	type, extends(base) :: parent
 		contains
 		procedure, nopass :: bind => proc2
		end type

      type, extends(parent) :: child 
      contains
      procedure, nopass :: bind => proc3
      end type

      contains
      integer function proc1(arg1)
			type(child), intent(in) :: arg1
         proc1 = arg1%x
      end function proc1

      integer function proc2(arg1)
     	   type(child), intent(in) :: arg1
         proc2 = arg1%x 
      end function proc2

      integer function proc3(arg1)
         type(child), intent(in) :: arg1
         proc3 = arg1%x 
      end function proc3
	end module     

   use mod

   type(child) :: base_dt1
   type(child) :: parent_dt1
   type(child) :: child_dt1

   base_dt1%x = 10
   parent_dt1%x = 30
   child_dt1%x = 50

   if(base_dt1%bind(base_dt1) .ne. base_dt1%x)   error stop 2
   if(parent_dt1%bind(parent_dt1) .ne. parent_dt1%x) error stop 3
   if(child_dt1%bind(child_dt1) .ne. child_dt1%x) error stop 4

   end
   
