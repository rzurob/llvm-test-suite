! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/F2003/mv_Alloc/typCompatible/diffDTpolyDT1.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self -qreuse=base

! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : diffDTpolyDT1.f 
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
!*  DESCRIPTION                : TO is of a parent poly DT
!*                               FROM is of a child non-poly DT, component of DT
!*                               integer pointer as component of parent DT
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      type A(n1,k1)    ! (20,4)
          integer, kind        :: k1
          integer, len         :: n1
          integer(k1), pointer :: ip => null() 
      end type 
      
      type , extends(A) :: B(k2,n2)    ! (20,4,4,20)
          integer, kind               :: k2
          integer, len                :: n2
          class(A(:,k2)), allocatable :: p 
      end type 

      type(A(:,4)), allocatable ::  a1
      class(A(:,4)), allocatable :: a2 
      
      type(B(20,4,4,20)) ::  b1 
      type(B(:,4,4,:)), allocatable :: b2

      integer , target :: id, id1, id2, id3

      id = 11
      allocate(a1, source = A(20,4)(id))

      id1 = 21
      allocate(a2, source = A(20,4)(id1))
  
      id2 = 31 
      allocate(b2, source = B(20,4,4,20)(id2,a2)) 

      id3 = 41
      b1 = B(20,4,4,20)(id3, a1)

      call move_alloc(b2, b1%p)

      if ( allocated(b2 ) ) stop 11
      if ( .not. allocated(b1%p) ) stop 20

      select type (x => b1%p)
          type is (B(*,4,4,*))
              if ( x%ip /= 31 ) stop 21
              select type ( y => x%p)
                  type is (A(*,4))
                      if ( y%ip /= 21 ) stop 23
                  class default 
                      stop 25
              end select
          class default
              stop 31
      end select
      end
