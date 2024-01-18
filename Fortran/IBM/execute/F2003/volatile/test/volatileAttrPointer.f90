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
!*        usually a pointer should have the VOLATILE attribute if its
!*     target has the VOLATILE attribute, all members of an EQUIVALENCE
!*     should have the VOLATILE attribute if one member has
!*     the VOLATILE attribute
!* ===================================================================

  program volatileAttrPointer 

    integer, target::x(3)
   
    integer, pointer::y(:)

    VOLATILE x 

    y=> x

  end program volatileAttrPointer 
