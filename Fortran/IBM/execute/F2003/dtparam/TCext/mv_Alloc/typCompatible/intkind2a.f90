! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/F2003/mv_Alloc/typCompatible/intkind2a.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

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
          type A(n1,k1)    ! (20,2)
              integer, kind              :: k1
              integer, len               :: n1
              integer(k1), allocatable     :: j1(:)
          end type
          type base(n2,k2)    ! (20,2)
              integer, kind              :: k2
              integer, len               :: n2
              integer(k2), allocatable     :: i1(:)
          end type
      end module

      module n
        use m
        type child(k3,n3,k4)    ! (4,20,2)
            integer, kind               :: k3,k4
            integer, len                :: n3
            type(base(n3,k4))           :: b1
            class(A(:,k4)), allocatable :: a1
        end type
      end module  


      use n

      class(child(4,:,2)), pointer :: p1

      integer(2), allocatable :: x(:), y(:)
      allocate(x(3), source = (/ 30_2, 20_2, 10_2 /) )

      allocate(p1, source = child(4,20,2)( base(20,2)(x), A(20,2)(y)) )

      associate ( x => p1%b1 ) 
          associate ( y => p1%a1 )
              call move_alloc(x%i1, y%j1)
              if ( allocated(x%i1) ) stop 21 
              if ( .not. allocated(y%j1) ) stop 23
          end associate
      end associate

      print *,  p1%a1%j1

      end
