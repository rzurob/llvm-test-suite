! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/14/2006
!*
!*  DESCRIPTION                : Testing empty array constructors.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      integer :: i = 5
      print *, len([ character(i) :: ])
      end