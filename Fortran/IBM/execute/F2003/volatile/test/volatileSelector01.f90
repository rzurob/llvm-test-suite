!*  ===================================================================
!*
!*  DATE                       : 30/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : selector, VOLATILE
!*
!*  DESCRIPTION                : functional TC
!*
!*     8.1.4.3
!*
!*  The associating entity has the ASYNCHRONOUS, TARGET, or VOLATILE attribute
!*  if and only if the selector is a variable and has the attribute.
!* ===================================================================

  program volatileSelector01

   integer, VOLATILE:: x
   x = 8

   associate(As => x)
       print *, As
   end associate

  end program volatileSelector01
