! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self -qreuse=base /tstdev/OO_type/abstract/C611/structComp003.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

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
   
   type, abstract :: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: id
   end type
   
   type, extends(base) :: child    ! (4)
      real(k1) :: rid
   end type
   
   type :: otherbase1(k2)    ! (4)
      integer, kind            :: k2
      class(base(k2)), pointer :: ptr
   end type
   
   type :: otherbase2(k3)    ! (4)
      integer, kind                :: k3
      class(base(k3)), allocatable :: alloc
   end type
   

end module

program structComp003
   use m  

   type(child(4)), target :: c1
   class(child(4)), allocatable, target :: c2
   type(otherbase1(4)) :: ob1
   class(otherbase2(4)), allocatable :: ob2
      
   allocate(ob2, c2)
   
   ob1%ptr => c1
   ob1%ptr => c1%base
   
   allocate (ob2%alloc, source = c1)
   deallocate( ob2%alloc)
   allocate (ob2%alloc, source = c1%base)
   
   ob1%ptr => c2%base
   allocate( ob1%ptr, source = c2%base)
   
end program
