! GB DTP extension using:
! ftcx_dtp -ql -qreuse=none /tstdev/OO_tpbnd/specbnd/ftybn092e.f
! opt variations: -qnol -qreuse=base

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod  
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f 
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: dcomp ftybn092e.f ftybn092e.vf 
! %END
!**********************************************************************
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn092e.f 
!*  TEST CASE TITLE            : type-bound procedure
!*
!*  PROGRAMMER                 : Catherine Sun
!*  DATE                       : 
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute 
!*
!*  SECONDARY FUNCTIONS TESTED : accessiblity  
!*
!*  DESCRIPTION                : chang the accessibility of the 
!*                               type-bound procedures by overriding
!*                               it in an extended type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      module mod	      

      type base(n1,k1)    ! (20,4) 
         integer, kind :: k1
         integer, len  :: n1
         integer(k1)   :: x
         contains
      	 procedure, nopass,public :: bind_b => proc1
      end type 

      type, extends(base) :: child(n2,k2)    ! (20,4,20,4) 
         integer, kind :: k2
         integer, len  :: n2
         integer(k2)   :: y
      contains
!* will issure an error message here
         procedure, nopass, private :: bind_b => proc1
      end type  

      contains
      subroutine proc1()
      end subroutine

   end module     


   end
   
