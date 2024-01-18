! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/mv_Alloc/typCompatible/mixunlmtpolyDT3.f
! opt variations: -qck -qnol -qnodeferredlp

! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : mixunlmtpolyDT3.f 
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
!*  DESCRIPTION                : FROM is of an nonpoly DT 
!*                               TO is of unlmited poly
!*                               both are dummy args
!*                               defect 322393
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

      type  :: base(n1,k1)    ! (20,8)
          integer, kind :: k1
          integer, len  :: n1
          integer(k1)   :: id
          character(:), allocatable :: name
      end type 

      contains
          subroutine sub(a, b)
              type(base(:,8)), allocatable :: a(:,:)
              class(*), allocatable  :: b(:,:)

              call move_alloc(a, b)
          end subroutine

end module

use m
      integer i
      type(base(:,8)), allocatable :: a(:,:)
      class(*), allocatable :: b(:,:)
      allocate(a(5,2), source=reshape((/ (base(20,8)(i, char(i+70)), i=1,10) /) , &
           (/5, 2/) ))
      call sub(a, b)

      if ( allocated(a) ) stop 21
      if ( .not. allocated(b)) stop 23
      select type(b)
          type is (base(*,8))
!              print *, b(1,1)%id, b(2,1)%id, b(3,1)%id, b(4,1)%id, b(5,1)%id 
!              print *, b(1,1)%name, b(2,1)%name, b(3,1)%name, b(4,1)%name, b(5,1)%name
!              print *, b(1,2)%id, b(2,2)%id, b(3,2)%id, b(4,2)%id, b(5,2)%id 
!              print *, b(1,2)%name, b(2,2)%name, b(3,2)%name, b(4,2)%name, b(5,2)%name
              print *,  (/ ( b(i,1)%id, i = 1,5) /) 
              print *,  (/ ( b(i,1)%name, i = 1,5) /) 
              print *,  (/ ( b(i,2)%id, i = 1,5) /) 
              print *,  (/ ( b(i,2)%name, i = 1,5) /) 
          class default
             stop 51
      end select

      end
