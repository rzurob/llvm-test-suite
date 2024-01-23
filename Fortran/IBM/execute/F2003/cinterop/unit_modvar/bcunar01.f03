! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : May. 24, 2003
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    : -qfree=f90
!*
!* DESCRIPTION                  : Test bind(c) can be treat as array.
!234567890123456789012345678901234567890123456789012345678901234567890
      subroutine sub1
      integer c,k
      bind(c)=c+1
      k=bind(5)
      print*,k
      end
      subroutine sub2
      integer pascal,k
      bind(pascal)=pascal+1
      k=bind(5)
      print*,k
      end
      call sub1
      call sub2
      end
