! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_tpbnd/specbnd/ftybn021c.f
! opt variations: -qnok -qnol

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod 
! %COMPOPTS: -qfree=f90 
! %GROUP: ftybn021c.f
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
!*  TEST CASE NAME             : ftybn021c.f 
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
!*  DESCRIPTION                : public the private binding procedure of  
!*                               parent in extended type. Binding procedure
!*                               with dummy arguments.
!*                             
!*                            
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


	module mod	      
   private
		type, private :: base(n1,k1)    ! (20,4) 
      integer, kind       :: k1
      integer, len        :: n1
      integer(k1), public :: x
		contains
      procedure, nopass :: bind => proc1
		end type base

   	type, extends(base), public :: parent(k2,n2)    ! (20,4,4,20)
   	    integer, kind :: k2
   	    integer, len  :: n2
 		contains
      procedure, nopass :: bind => proc2
 		procedure, nopass :: bind_parent => proc2
		end type

      type, extends(parent), public :: child(k3,n3)    ! (20,4,4,20,4,20) 
          integer, kind :: k3
          integer, len  :: n3
      contains
      procedure, nopass :: bind => proc3
      procedure, nopass :: bind_child => proc3
      end type

      contains
      integer function proc1(arg1)
      type(child(*,4,4,*,4,*)) :: arg1
         proc1 = arg1%x 
      end function proc1

      integer function proc2(arg1)
      type(child(*,4,4,*,4,*)) :: arg1
         proc2 = 2 * arg1%x 
      end function proc2

      integer function proc3(arg1)
      type(child(*,4,4,*,4,*)) :: arg1
         proc3 = 3 * arg1%x 
      end function proc3
	end module     

   use mod

   type(child(20,4,4,20,4,20)) :: child_dt1, child_dt2, child_dt3

   child_dt1%x = 100
   child_dt2%x = 100
   child_dt3%x = 100

   if(child_dt1%bind(child_dt1) .ne. 3 * child_dt1%x)   error stop 2
   if(child_dt1%bind_child(child_dt1) .ne. 3 * child_dt1%x) error stop 3
   if(child_dt1%bind_parent(child_dt1) .ne.2 * child_dt1%x) error stop 4
 
! print *, base_dt1%bind() 
! print *, parent_dt1%bind()
! print *, child_dt1%bind()

  end
   
