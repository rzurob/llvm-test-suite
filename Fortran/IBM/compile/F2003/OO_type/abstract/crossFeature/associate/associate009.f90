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
! %POSTCMD: dcomp associate009.f                                                     
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
!*  DESCRIPTION                : Testing:  Associate Construct
!*                                         a) Associate-name associating with scalar strucuture constructor
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

module m
   
   type, abstract :: base
      integer :: id
   end type
   
   type, extends(base) :: child
   end type

end module

program associate009
   use m
   
   associate ( myBase => base(5) )
      error stop 1_4
   end associate
            
end program
