! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/volatile/test/volatileMisc02.f
! opt variations: -ql

!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 06/06/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :  VOLATILE
!*
!*  DESCRIPTION                : 
!*          VOLATILE should be specified before  the derived type name
!*     is used.              
!234567890123456789012345678901234567890123456789012345678901234567890
 
   program volatileMisc02

   type volatile(k1)    ! (4)
     integer, kind :: k1
     integer(k1)      volatile
   end type

   type(volatile(4)), volatile :: volatile

   end program volatileMisc02
 
