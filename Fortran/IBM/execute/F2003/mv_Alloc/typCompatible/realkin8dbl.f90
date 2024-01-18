! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : realkin8dbl.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/13/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*                              
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : FROM is of real dbl precision
!*                               TO is of type real*8 
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

      double precision, public, dimension(:,:,:,:,:) :: x
      real*8, private, dimension(:,:,:,:,:) :: y

      allocatable x, y

      contains
          subroutine sub()
              call move_alloc(x,y)
          end subroutine

          subroutine sub1()
              if ( .not. allocated(y)) stop 51
              if ( allocated(x)) stop 53

              print*, lbound(y,1),lbound(y,2),lbound(y,3),lbound(y,4),lbound(y,5)

              print*, ubound(y,1),ubound(y,2),ubound(y,3),ubound(y,4),ubound(y,5)

              write (*, '(2f15.10)') x(1,2,3,4,5), x(0,1,2,3,4)

          end subroutine
end module

      use m

      allocate(x(0:1,1:2,2:3,3:4,4:5))

      x(0::2,1::2,2::2, 3::2, 4::2 ) = 4.3_8
      x(1::2,2::2,3::2, 4::2, 5::2 ) = 2.7_8

      call sub

      call sub1

    end
