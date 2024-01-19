!*  ===================================================================
!*
!*  DATE                       : 06/06/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :  VOLATILE
!*
!*  DESCRIPTION                :
!*          VOLATILE should be specified before  the derived type name
!*     is used.
!234567890123456789012345678901234567890123456789012345678901234567890

   program volatileMisc02d

   type volatile
     integer volatile
   end type

   type(volatile) :: volatile

   VOLATILE :: volatile

   end program volatileMisc02d

