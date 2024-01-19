! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/volatile/test/volatileMisc02d.f
! opt variations: -ql

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

   type volatile(k1)    ! (4)
     integer, kind :: k1
     integer(k1)      volatile
   end type

   type(volatile(4)) :: volatile

   volatile :: volatile

   end program volatileMisc02d

