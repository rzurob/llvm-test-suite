! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : TO is of complex(4)
!*                               FROM is of complex*8A
!*                               scalar
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


     program main

          complex*8 c1
          complex(kind=4) :: c2
          allocatable c1,c2
          logical precision_x8

          allocate(c1)
          c1 = (2.d1, 2.d0)

          call move_alloc(c1, c2)

          if ( .not. allocated(c2) ) error stop 21
          if ( allocated(c1) ) error stop 31
          if ( .not. precision_x8 (c2, (20.0000,2.0000) )) error stop 51_4

     end