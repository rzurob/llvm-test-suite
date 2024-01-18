! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : intkind2a.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/01/2006
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
!*  DESCRIPTION                : FROM/TO are of type integer(2) 
!*                               called in associate construct
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
              integer(2), allocatable   ::  j1(:)
          end type
          type base
              integer(2), allocatable   ::  i1(:)
          end type
      end module

      module n
        use m
        type child
            type(base) :: b1
            class(A), allocatable :: a1
        end type
      end module  


      use n

      class(child), pointer :: p1

      integer(2), allocatable :: x(:), y(:)
      allocate(x(3), source = (/ 30_2, 20_2, 10_2 /) )

      allocate(p1, source = child( base(x), A(y)) )

      associate ( x => p1%b1 ) 
          associate ( y => p1%a1 )
              call move_alloc(x%i1, y%j1)
              if ( allocated(x%i1) ) stop 21 
              if ( .not. allocated(y%j1) ) stop 23
          end associate
      end associate

      print *,  p1%a1%j1

      end
