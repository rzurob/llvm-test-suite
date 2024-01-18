!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d356494.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Sept. 20 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*
!* 1. DEFECT 356494 
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type :: dtp
     integer(4)  :: i[1]=[1]  !<== syntax error
   end type
end module

program d356494
   use m
   implicit none

   type(dtp),parameter  :: dtp1(1)=[dtp()]

end program

