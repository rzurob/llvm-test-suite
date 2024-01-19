! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : TO is component of a derived-type, unlimited poly
!*                               FROM is of complex*16
!*                               rank 3
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

       type A
          complex*16, allocatable :: a1(:,:,:)
          class(*), allocatable :: a2(:,:,:)
       end type

end module

program main

       use m
       class(*), pointer :: p

       allocate( A :: p)

       select type(p)
           type is (A)
               allocate( p%a1(-1:2,2:1,2) )
               p%a1 = cmplx( real(3,4), real(1,8), 8)
               allocate ( complex(8) :: p%a2(256,1,1) )
               call move_alloc(p%a1, p%a2 )

               if (  allocated(p%a1) ) error stop 21
               if (  .not. allocated(p%a2) ) error stop 23
               if ( size(p%a2,1) /= 4 ) ERROR STOP 41
               if ( size(p%a2,2) /= 0 ) ERROR STOP 43
               if ( size(p%a2,3) /= 2 ) ERROR STOP 45

       end select


end


