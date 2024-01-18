!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 30/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : subobject, VOLATILE
!*
!*  DESCRIPTION                : functional TC  
!*
!*     5.1.2.16 
!*        an object may have the volatile attribute in a particular scoping 
!*        unit without necessarily having it in other scoping units. If
!*        an object has the Volatile attribute, then all of its subobject
!*        also have the VOLATILE attribute.
!* ===================================================================

  program volatileSubobject02

    complex x
    real    a, b
    VOLATILE::x
    
    a = 2.0
    b = 3.0
  
    x = (a, b)

  end program volatileSubobject02
