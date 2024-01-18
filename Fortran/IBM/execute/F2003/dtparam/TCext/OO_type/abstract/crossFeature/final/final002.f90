! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/final/final002.f
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
   type , abstract:: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
   end type
   
   type, extends(base) :: child(k2,n1)    ! (4,4,20)
       integer, kind :: k2
       integer, len  :: n1
   contains
      final :: finalchild1, finalchild2
   end type
   
contains

   subroutine finalchild1(a)
      type(child(4,4,*)), intent(inout) :: a
      print *,"finalizechildscalar: ", a%i
      a%i = 0
   end subroutine
   
   subroutine finalchild2(a)
      type(child(4,4,*)), intent(inout) :: a(:)
      print *,"finalizechildarray: ", a%i
      a%i = 0
   end subroutine
   
end module

program final002
   use m
     
   class(base(4)), allocatable :: b1
   class(base(4)), allocatable, dimension(:) :: b2
   
   allocate (b1, source = child(4,4,20)(5))
   allocate (b2(2), source = (/child(4,4,20)(1),child(4,4,20)(2)/))
   
   deallocate(b1,b2)
            
end program
   
   
