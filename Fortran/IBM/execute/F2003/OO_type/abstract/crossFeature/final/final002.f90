!######################################################################
! SCCS ID Information                                                  
! %W%, %I%                                                             
! Extract Date/Time: %D% %T%                                           
! Checkin Date/Time: %E% %U%                                           
!######################################################################
! *********************************************************************
! %START                                                               
! %MAIN: YES                                                           
! %PRECMD: rm -f *.mod                                                 
! %COMPOPTS: -qfree=f90                                                
! %GROUP: final002.f                                                 
! %VERIFY: final002.out:final002.vf                                                  
! %STDIN:                                                              
! %STDOUT: final002.out                                                           
! %EXECARGS:                                                           
! %POSTCMD:                                                      
! %END                                                                 
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 09/28/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Final Subroutines
!*                               Define Multiple Final Subroutine for extension type of abstract type
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type , abstract:: base
      integer :: i
   end type
   
   type, extends(base) :: child
   contains
      final :: finalchild1, finalchild2
   end type
   
contains

   subroutine finalchild1(a)
      type(child), intent(inout) :: a
      print *,"finalizechildscalar: ", a%i
      a%i = 0
   end subroutine
   
   subroutine finalchild2(a)
      type(child), intent(inout) :: a(:)
      print *,"finalizechildarray: ", a%i
      a%i = 0
   end subroutine
   
end module

program final002
   use m
     
   class(base), allocatable :: b1
   class(base), allocatable, dimension(:) :: b2
   
   allocate (b1, source = child(5))
   allocate (b2(2), source = (/child(1),child(2)/))
   
   deallocate(b1,b2)
            
end program
   
   