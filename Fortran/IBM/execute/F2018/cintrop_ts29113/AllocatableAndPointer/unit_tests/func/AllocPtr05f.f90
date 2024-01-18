!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AllocPtr05f
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Feb, 2013
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop Allocatable/Pointer
!*                                                   
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf2008
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*
!*  DESCRIPTION                : Doing Fortran I/O when CFIs are involved
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: iso_c_binding
      implicit none

      interface
        subroutine s1(arg) bind(c)
          import
          real(c_float), pointer :: arg(:,:)
        end
        subroutine s2(arg) bind(c)
          import
          integer(c_int), pointer :: arg
        end
        subroutine s3(arg) bind(c)
          import
          integer(c_int), allocatable :: arg(:,:)
        end
        subroutine s4(arg) bind(c)
          import
          real(c_float), allocatable :: arg
        end
      end interface

      real(c_float), pointer :: p1(:,:)
      integer(c_int), pointer :: p2
      integer(c_int), allocatable :: al1(:,:)
      real(c_float), allocatable :: al2

      real(c_float), target :: t1(3,2)
      integer(c_int), target :: t2 = 4321

      t1(1,1) = 1.0; t1(1,2) = 2.0;
      t1(2,1) = 3.0; t1(2,2) = 4.0;
      t1(3,1) = 5.0; t1(3,2) = 6.0;
      
      p1 => t1
      p2 => t2

      allocate(al1(3,2))
      al1 = -INT(t1)
      allocate(al2)
      al2 = -REAL(4321)

      call s1(p1)
      call s2(p2)
      call s3(al1)
      call s4(al2)

      end

      subroutine s1(arg) bind(c)
        use, intrinsic :: iso_c_binding
        real(c_float), pointer :: arg(:,:)
        print '(6F6.2)', arg
        print '(2F6.2)', arg(1,2), arg(2,1)
      end
      subroutine s2(arg) bind(c)
        use, intrinsic :: iso_c_binding
        integer(c_int), pointer :: arg
        print *, arg
      end
      subroutine s3(arg) bind(c)
        use, intrinsic :: iso_c_binding
        integer(c_int), allocatable :: arg(:,:)
        print *, arg
        print *, arg(1,2), arg(2,1)
      end
      subroutine s4(arg) bind(c)
        use, intrinsic :: iso_c_binding
        real(c_float), allocatable :: arg
        print '(F9.2)', arg
      end  
