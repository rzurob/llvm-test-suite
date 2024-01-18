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
! %GROUP: redherring.f                                                 
! %VERIFY:                                                             
! %STDIN:                                                              
! %STDOUT:                                                             
! %EXECARGS:                                                           
! %POSTCMD: dcomp structComp003.f                                           
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
!*  DESCRIPTION                : Testing: If the rightmost part-name is of abstract type, data-ref shall be polymorphic. (C611)
!*                                        polymorphic abstract type data-ref assigned pointer assignment, allocate statement
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
   
   type, abstract :: base
      integer :: id
   end type
   
   type, extends(base) :: child
      real :: rid
   end type
   
   type :: otherbase1
      class(base), pointer :: ptr
   end type
   
   type :: otherbase2
      class(base), allocatable :: alloc
   end type
   

end module

program structComp003
   use m  

   type(child), target :: c1
   class(child), allocatable, target :: c2
   type(otherbase1) :: ob1
   class(otherbase2), allocatable :: ob2
      
   allocate(ob2, c2)
   
   ob1%ptr => c1
   ob1%ptr => c1%base
   
   allocate (ob2%alloc, source = c1)
   deallocate( ob2%alloc)
   allocate (ob2%alloc, source = c1%base)
   
   ob1%ptr => c2%base
   allocate( ob1%ptr, source = c2%base)
   
end program