!* =================================================================== &
!* XL Fortran Test Case                          IBM INTERNAL USE ONLY
!* =================================================================== &
!*
!* TEST CASE TITLE            : intrinsictype17f.F
!*
!* PROGRAMMER                 : David Nichols
!* DATE                       : March 10, 2011
!* ORIGIN                     : AIX Compiler Development,
!*                            : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : Intrinsic types in TYPE spec 
!*
!* DRIVER STANZA              : xlf2008
!*
!* DESCRIPTION                : Testing proper functionality of
!*                              vector types in TYPE spec 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program intrinsictype17f

      TYPE ( vector(unsigned) ) ::   vu
      TYPE ( vector(unsigned*4) ) ::   vu4
      TYPE ( vector(unsigned(2)) ) ::   vu2

      TYPE ( vector(pixel) ) ::   vp

      type ( vector(real) ) ::   vr
      type ( vector(real*4) ) ::   vr4
      type ( vector(real(8)) ) ::   vr8

      type ( vector(integer) ) ::   vi
      type ( vector(integer*4) ) ::   vi4
      type ( vector(integer(8)) ) ::   vi8

      end
