!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 01/11/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 330415)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      complex(16) :: cx3
      real(8)  :: rl3requiv, rl3iequiv
      real(16) :: rl3r, rl3i

      equivalence(rl3r, rl3requiv)
      equivalence(rl3i, rl3iequiv)

      rl3requiv = z'7FF0000000000000' ! positive Inf
      rl3iequiv = z'7FF0000000000000'
      cx3 = (rl3r, rl3i)

      print *, rl3r, rl3i
      print *, cx3
      end

