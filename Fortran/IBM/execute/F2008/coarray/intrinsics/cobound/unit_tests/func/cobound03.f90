!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : cobound03
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Aug 20, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Functionality of cobound intrinsics
!*                                                   
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : Testing various combinations of
!*                               argument keywords for the cobound
!*                               intrinsics.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none
      integer, save :: coarr(5)[10,3:4,*]
      integer(kind=4) :: i4
      integer(kind=8) :: i8
      integer(kind=8) :: i8arr(3)
      integer :: i

      if (this_image() == 1) then

         print *, lcobound(COARRAY=coarr)
         print *, lcobound(KIND=8, COARRAY=coarr)
         print *, ucobound(KIND=4, COARRAY=coarr, DIM=1)
         print *, ucobound(KIND=8, DIM=2, COARRAY=coarr)

         i8 = ucobound(coarr, KIND=8, DIM=1)
         if (i8 .ne. 10_8) error stop 1

         i8 = ucobound(coarr, KIND=8, DIM=2)
         if (i8 .ne. 4_8) error stop 2

         i8arr = lcobound(KIND=8, COARRAY=coarr)
         if (any(i8arr .ne. [1_8,3_8,1_8])) error stop 3

         i4 = ucobound(coarr, DIM=1, KIND=4)
         if (i4 .ne. 10_4) error stop 4

         i4 = ucobound(coarr, DIM=2, KIND=4)
         if (i4 .ne. 4_4) error stop 5

         i  = ucobound(DIM=1, COARRAY=coarr)
         if (i .ne. 10) error stop 6
         
      end if

      sync all

      end
