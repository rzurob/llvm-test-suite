! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/mv_Alloc/typCompatible/dblcmplx.f
! opt variations: -qnok -qnol

! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : dblcmplx.f 
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
!*  DESCRIPTION                : FROM/TO are of type double complex
!*                               see defect 321912
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      type  :: base(k1,n1)    ! (4,20)
          integer, kind :: k1
          integer, len  :: n1
          sequence
          double complex, allocatable :: d1(:,:,:), d2(:,:,:)
      end type 
      type(base(4,20)) :: b

      interface 
           subroutine sub(arg,brg)
                  type :: base(k1,n1)    ! (4,20)
                      integer, kind :: k1
                      integer, len  :: n1
                      sequence 
                      double complex, allocatable :: d1(:,:,:), d2(:,:,:)
                  end type
                  type(base(4,*)) brg
                  double complex, optional, allocatable :: arg(:,:,:)
           end subroutine
      end interface

      allocate(b%d2(2,3,1))

      b%d2(:,:,:) = reshape ([1,2,3,4,5,6], [2,3,1])

      call sub(b%d2, b)

      if (allocated (b%d2)) stop 25

      if (.not. allocated(b%d1)) stop 26
      if ( any ([b%d1(:,:,:)] /= [1,2,3,4,5,6])) stop 22
      if ( any (shape(b%d1) /= [2, 3, 1])) stop 24

      end

      subroutine sub( arg, brg )
          type :: base(k1,n1)    ! (4,20)
              integer, kind :: k1
              integer, len  :: n1
              sequence 
              double complex, allocatable :: d1(:,:,:), d2(:,:,:)
          end type
          type(base(4,*)) brg
          double complex, optional, allocatable :: arg(:,:,:)

          call move_alloc(arg,brg%d1) 
                  
          if ( .not. allocated(brg%d1)) stop 21
          if ( allocated(arg) ) stop 23 
      end subroutine
