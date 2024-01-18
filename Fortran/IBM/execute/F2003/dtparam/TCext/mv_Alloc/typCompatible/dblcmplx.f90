! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/mv_Alloc/typCompatible/dblcmplx.f
! opt variations: -qnok -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
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

      if (allocated (b%d2)) error stop 25

      if (.not. allocated(b%d1)) error stop 26
      if ( any ([b%d1(:,:,:)] /= [1,2,3,4,5,6])) error stop 22
      if ( any (shape(b%d1) /= [2, 3, 1])) error stop 24

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

          if ( .not. allocated(brg%d1)) error stop 21
          if ( allocated(arg) ) error stop 23
      end subroutine
