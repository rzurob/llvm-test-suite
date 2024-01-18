! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : TO is of unlimit poly
!*                               FROM is of complex(4)
!*                               rank 1
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


     program main

          complex(4) :: c1(:)
          allocatable c1
          class(*), allocatable :: c2(:)

          logical precision_x8
          allocate(complex*16  :: c2(5) )

          allocate(c1(2),source=(/cmplx(1.d1,1.d0,4),cmplx(2.d1,1.d0,4)/))

          call move_alloc(c1, c2)

          if ( .not. allocated(c2) ) stop 21
          if ( allocated(c1) ) stop 31

          select type (c2)
               type is (complex*8)
                  if ( size(c2) /= 2) stop 11
                  if ( .not. precision_x8 (c2(1), (10.0000,1.0000) )) error stop 51_4
                  if ( .not. precision_x8 (c2(2), (20.0000,1.0000) )) error stop 61_4
               class default
                   stop 31
          end select
end
