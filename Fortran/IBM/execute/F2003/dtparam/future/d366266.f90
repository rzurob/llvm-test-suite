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
!*  DATE                       : 2009-11-29
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : defect 366266
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      TYPE Base (k1,l1)
        INTEGER, KIND :: k1 = 4
        INTEGER, LEN  :: l1 = 10
      END TYPE

      TYPE :: Child (k2,l2)
        INTEGER, KIND :: k2 = 4
        INTEGER, LEN  :: l2 = 5

        TYPE(Base(k2,k2)) :: arr(k2) = [Base(k2,k2) :: (Base(k2,k2) ( ), I = 1,k2) ]
        !TYPE(Base(k2,k2)) :: arr(4) = [Base(k2,k2) :: (Base(k2,k2) ( ), I = 1,4) ] !<-- use this line tc passes
      END TYPE

      TYPE(Child(4,5)) :: c1
END

