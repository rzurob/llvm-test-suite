! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/mv_Alloc/typCompatible/intkind1.f
! opt variations: -qnol

! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : intkind1.f
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
!*  DESCRIPTION                : FROM/TO are of type integer(1) 
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


      module m
          integer(1), allocatable   ::  i1(:,:,:) 
          type base(n1,k1)    ! (20,1)
              integer, kind              :: k1
              integer, len               :: n1
              integer(k1), allocatable     :: i1(:,:,:) 
          end type
      end module


      use m

      type(base(20,1)) b1
      integer(1) i

      allocate(b1%i1(3,2,1), source = reshape((/(i,i=1_1,6_1)/),(/3,2,1/)))

      call move_alloc(b1%i1, i1)

      if ( allocated(b1%i1) ) stop 21
      if ( .not. allocated(i1) ) stop 31
      print *, i1(:,1,1)  
      print *, i1(:,2,1)  
      end
