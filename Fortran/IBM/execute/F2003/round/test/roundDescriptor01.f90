!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 24/07/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND descriptor 
!*                             
!*
!*  DESCRIPTION                : 
!*                            test round descriptor have precedence over
!*                            round specifiers.
!* ===================================================================

  program roundDescriptor01 

     real x, a
     complex y, b

     x = 1.3693555
     y = (23.4567855, 23.4567855)

     open(11, file='tstOne', form='formatted', round='down')

     write(11, '(RU, f8.6, 2x, 2f9.6)', round="zero") x, y

  end program roundDescriptor01 
